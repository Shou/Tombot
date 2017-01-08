
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeApplications,
             PartialTypeSignatures
#-}


module Tombot.Discord.Actions where


-- {{{ Imports
import Tombot.Discord.Types (Discord)
import qualified Tombot.Discord.Types as Discord
import Tombot.Funcs
import Tombot.Parser
import Tombot.Types
import Tombot.Utils

import Control.Lens as Lens
import Control.Monad
import Control.Monad.Except

import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Either
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (formatTime, getCurrentTime, defaultTimeLocale)
import Data.Vector (Vector)
import qualified Data.Vector as Vec

import Database.SQLite.Simple.Types (Null(..))

import Text.Regex
-- }}}


-- | Run the language interpreter on the message.
messageInterpret :: Message Discord -> Mind Discord ()
messageInterpret message = do
    functions <- Map.union funcs <$> serverfuncs funcs
    kawaiiLang <- botparse functions $ _messageContent message
    text <- compile functions kawaiiLang
    send <- sees _currSender

    send $ message & Lens.set messageContent text

-- | Lookup and print any tell messages.
messageTell :: Message Discord -> Mind Discord ()
messageTell message = do
    musers <- readLocalStored "tell"
    nick <- _userNick . _currUser & sees

    let cinick = CI.mk nick
        mtexts = join $ Map.lookup cinick <$> musers
        msgs = maybe [] id mtexts
        (msg, msgs') = pipeJoin msgs
        users = maybe (Map.singleton cinick msgs)
                      (Map.insert cinick msgs')
                      musers

    when (isJust msg) $ do
        modLocalStored "tell" $ const users

        send <- sees _currSender

        send $ message & Lens.set messageContent
                                  (nick <> ", " <> fromJust msg)

-- | Match against any regex responses and print them.
messageMatch :: Message Discord -> Mind Discord ()
messageMatch message = do
    (mlon :: Maybe _) <- readLocalStored "respond"
    (mgons :: Map String _) <- readGlobalStored "respond"

    let mons = mlon <> Just mgons
        ons :: [(String, (Bool, Int, String))]
        ons = sortBy (comparing $ snd3 . snd) $ maybe mempty Map.toList mons

    void . decide $ forM_ ons $ \(match, (ins, n, resp)) -> deci . decide $ do
        let regex = mkRegexWithOpts match False ins
            text = _messageContent message

        emins <- try $ return $! matchRegex regex $ Text.unpack text

        when (isLeft emins) $ do
            verb ("onMatch: " <> show emins)
            throwError ()

        let mins = either (const $ Just []) id emins

        unless (isJust mins) $ throwError ()

        let ins' = fromJust mins
            -- FIXME who needs indexes larger than 9 anyway lol
            indexes = [ '\\' : show x | x <- [0 .. 9]]
            replacer = zipWith replace indexes (Text.unpack text : ins')
            resp' = foldr ($) resp replacer

        let message' = message & Lens.set messageContent (Text.pack resp')

        lift $ messageInterpret message'
  where
    replace a b c = Text.unpack $ Text.replace (Text.pack a) (Text.pack b) (Text.pack c)
    deci :: Mind Discord (Either () ()) -> Decide Discord () ()
    deci m = lift m >>= either return throwError

-- | Add message to the logs.
messageLog :: Message Discord -> Mind Discord ()
messageLog message = do
    logPath <- _confLogPath <$> seeConfig
    time <- fmap Text.pack . liftIO $ do
        formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" <$> getCurrentTime

    let text = _messageContent message
        nick = _messageUserId message
        dest = _messageDestination message

    insertLog time text nick dest Null

messageTopicDetection :: Message Discord -> Mind Discord ()
messageTopicDetection message = do
    logs <- queryLogs @_ @(Text, Text, Text, Text, Maybe Text)

    let logMessages = map (view _2) logs
        document = Vec.fromList
                 $ map mkIdiom
                 $ Text.words
                 $ _messageContent message
        corpus = Vec.map (Vec.fromList . map mkIdiom . Text.words)
               $ Vec.fromList logMessages
        tfIdfs = Vec.map (\a -> tfIdf a document corpus) document
        tdIdfsMap = Map.fromList
                  $ zip (Vec.toList tfIdfs) (Vec.toList document)
        topFive = do
            (topKey, topElem) <- fst <$> Map.maxViewWithKey tdIdfsMap

            return $ unfoldr
                     (\prev -> uncurry (flip (,))
                            <$> Map.lookupLT prev tdIdfsMap)
                     topKey

    liftIO $ do
        putStr "Detection: "
        print tdIdfsMap

