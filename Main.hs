
-- This file is part of Tombot, licensed under the GNU GPL 2 license.
-- See the file "LICENSE" for more information.
-- Copyright Shou, 2013

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where


-- {{{ Imports
import Config
import Tombot.Funcs
import Tombot.IRC
import Tombot.Parser
import Tombot.Types
import Tombot.Utils

import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Concurrent.STM
import Control.Error hiding (left, right)
import Control.Exception (SomeException)
import qualified Control.Exception as E
import Control.Monad.State

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Language.Haskell.Interpreter as H

import Network

import System.Exit
import System.IO
import System.Timeout
-- }}}

-- XXX
-- - We are going to rename the bot?
--      - A name without `bot' in it.
-- - Store data as pure Haskell code in files, specifically for the Funcs.
--      - This means we don't need to make parsers and whatnot, we already have
--        GHC for that through Language.Haskell.Interpreter.
--      - There are no drawbacks other than requiring GHC to be installed, I
--        think?
--      - We can store functions there as well, which is great.
-- - Look for patterns and repetitions and make generic functions.

-- TODO
-- - Functions
--      - About
--      - Del
--      - .r or .reload for reloading modules.
-- - Logging
-- - Privilege system
--      - In progress
--          - Just need to review the functions
-- - Rejoin on Handle error
--      - Search for hPutStr and the likes
-- - Add quit and part messages to config data
-- - UTF-8 encoding
--      - #shrug
--      - make it SAFE
-- - Personality/mood
-- - Close Handles on reconnection.
--      - Make sure no Handle errors/reconnects are mistaken, such as the
--        reconnect loop one because two Eithers were joined.
-- - Run anything the bot writes through the parser. Or at least user commands.
--      - This means changes are made when the bot does something, such as NICK
--      - We've already set the bot up to be able to do this! `respond'!
-- - Message spam limit.
--      - Make a function that can only write 480 bytes of text to a Handle.
--      - Only n messages per m seconds.
--          - 5 messages per 3 seconds, drop everything after.
-- - Local IRC emulation.
-- - Keep track of UserStat
-- - Save lines to a file.
--      - This will allow users to do something like:
--        :on /abc(d)/ :load file -> ra
--          - This also means we have to replace the matches (\1, \2, ...)
--            AFTER, not before, we've compiled the kawaiilang.
--      - How to delete?
-- - Separate `help' file for functions defined by `:let'
--      - Local to channel
--      - Save it in same file as the funcs, "letfuncs"
-- - Separate `funcs' list for user defined functions.
--      - Write the `funcs' to a file "letfuncs" and then if it exists there,
--        the function is user defined. This is local to the channel.
--      - Local to channel
-- - Make it load the `Config' file on start so we don't need to recompile
--      - Alternatively load from a non-Haskell file.

-- FIXME
-- - handle Handle errors and rejoin.
-- - Add/modify channels on join
--      - Default Channel funcs.
--          - We don't want her to be completely useless in a channel, but no
--            dangerous funcs either; specifically network ones.
-- - Load UserStat from file.
--      - Now we just need to be able to set it somewhere.
-- - Get name/host on PRIVMSG.


-- TODO move this to Utils
-- XXX is it better to move hPutStrLn elsewhere?
-- | IRC network server connecting function that waits for a successful
-- connection and returns the Handle.
connecter :: MonadIO m => String -> PortNumber -> Text -> Text -> m Handle
connecter host port nick name = liftIO $ connecter' 0
  where
    connecter' :: Int -> IO Handle
    connecter' n = E.handle (onerror n) $ do
        verb $ "Connection delay by " <> show n <> " seconds"
        threadDelay (10^6 * n)
        mh <- timeout (10^7) $ do
            h <- connectTo host $ PortNumber port
            te <- mkTextEncoding "UTF-8//TRANSLIT"
            hSetEncoding h te
            hSetBuffering h LineBuffering
            hSetNewlineMode h (NewlineMode CRLF CRLF)
            T.hPutStrLn h $ "NICK " <> nick
            T.hPutStrLn h $ "USER " <> name <> " 0 * :" <> name
            T.hPutStrLn h $ "CAP REQ :multi-prefix"
            return h
        maybe (connecter' $ inctime n) pure mh
    inctime n = min 300 $ max n 1 * 2
    onerror :: Int -> SomeException -> IO Handle
    onerror n e = erro e >> connecter' (inctime n)

-- | Mind IRC server connector.
connect :: Mind ()
connect = do
    server <- sees currServ
    let host = stServHost server
        port = stServPort server
        nick = stServBotNicks server !! 0
        name = stServBotName server
    verb $ "Connecting to " <> host
    h <- connecter host port nick name
    mapConfig $ \c -> c { stConfHandles = M.insert host h $ stConfHandles c }
    sets $ \k -> k { currServ = server { stServHandle = h } }

-- | Reconnect to the IRC network server.
reconnect :: Mind ()
reconnect = do
    h <- sees $ stServHandle . currServ
    liftIO $ hClose h
    connect

-- | Initialise a Mind state.
initialise :: Mind ()
initialise = connect >> listen

-- TODO on Handle read exception
-- | Read the IRC handle.
listen :: Mind ()
listen = forever $ do
    current <- see
    let configt = currConfigTMVar current
        server = currServ current
        h = stServHandle server
        tryGetLine :: Mind (Maybe Text)
        tryGetLine = liftIO $ do
            fmap join $ timeout (240*10^6) $ fmap hush $ try $ T.hGetLine h
    mline <- do
        verb "Reading from Handle..."
        tryGetLine
    maybe reconnect respond mline
    liftIO $ verb "Successfully read from the Handle!"

-- | please
respond :: Text -> Mind ()
respond line = do
    server <- sees currServ
    let eirc = note line $ A.maybeResult $ A.parse ircparser line `A.feed` ""
    flip (either warn) eirc $ \irc -> void . runEitherT $ do
        liftIO $ print irc
        onNick irc $ \_ -> do
            nickUserlist irc
        onMode irc $ \_ -> do
            changeMode irc
        onQuit irc $ \_ -> do
            quitUserlist irc
        onJoin irc $ \_ -> do
            adaptJoin irc
            modeJoin irc
            joinUserlist irc
            remind irc
            greet irc
        onPart irc $ \_ -> do
            adaptPart irc
            partUserlist irc
        onTopic irc $ \(Topic nick name host c t) -> do
            void $ modChanTopic c $ const t
        onPing irc $ \(Ping t) -> write $ "PONG :" <> t
        onPrivmsg irc $ \_ -> do
            adaptPriv irc
            ctcpVersion irc
            printTell irc
            runLang irc
            onMatch irc
        onInvite irc $ \_ -> do
            joinInv irc
        -- XXX see if we can find a Numeric printed after the MOTD instead
        onNumeric "001" irc $ \_ -> do
            welcomeNum irc
        onNumeric "311" irc $ \_ -> do
            whoisNum irc
        onNumeric "324" irc $ \_ -> do
            modeNum irc
        onNumeric "332" irc $ \_ -> do
            topicNum irc
        onNumeric "353" irc $ \_ -> do
            adaptNum irc
            userlistNum irc
        onNumeric "482" irc $ \_ -> do
            privilegeNum irc

main :: IO ()
main = do
    configt <- newTMVarIO $ toStConf config
    forM_ servers $ \server -> forkIO $ do
        let host = servHost server
            port = fromIntegral $ servPort server
            nick = T.pack $ servBotNicks server !! 0
            name = T.pack $ servBotName server
        h <- connecter host port nick name
        let stServer = toStServ h server
            current = Current { currUser = defUser
                              , currMode = ""
                              , currServ = stServer
                              , currDest = Left defUser
                              , currConfigTMVar = configt
                              }
        currt <- newTMVarIO current
        void $ runStateT initialise currt
    userInput Nothing configt

-- XXX this is a mess, make it GOOD and not BAD
-- TODO quit
-- |
userInput :: Maybe String -> TMVar StConfig -> IO ()
userInput mhost tmvar = do
    line <- getLine
    mhs <- fmap stConfHandles $ atomically $ readTMVar tmvar
    let splitter x acc | x `isPrefixOf` line =
            let n = length x
            in Just (take n line, drop n line)
        splitter _ acc = acc
        mserver = foldr splitter Nothing $ M.keys mhs
        mserver' = mserver <|> fmap (,line) mhost
    let m = flip fmap mserver' $ \(host, msg) -> do
        when ("QUIT" `isPrefixOf` dropWhile (== ' ') msg) $ do
            forM_ (M.toList mhs) $ \(_, h) -> do
                let quitmsg = "QUIT :Something un-cute happened. :c"
                hPutStrLn h quitmsg
                verb quitmsg
            exitWith ExitSuccess
        let mh = M.lookup host mhs
            msg' = dropWhile (== ' ') msg
        unless (null msg') $ do
            let md = flip fmap mh $ \h -> do
                hPutStrLn h msg'
                putStrLn $ "\x1b[0;32m" <> msg' <> "\x1b[0m"
            maybe (return ()) id md
        return $ Just host
    emhost' <- try $ maybe (return Nothing) id m
    userInput (join $ hush emhost') tmvar

