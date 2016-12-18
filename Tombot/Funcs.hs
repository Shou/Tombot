
-- This file is part of Tombot, licensed under the GNU GPL 2 license.
-- See the file "LICENSE" for more information.
-- Copyright Shou, 2013

{-# LANGUAGE OverloadedStrings, DoAndIfThenElse, BangPatterns,
             TupleSections, ScopedTypeVariables, DeriveGeneric,
             PartialTypeSignatures, TypeOperators, TypeFamilies,
             TypeApplications #-}

module Tombot.Funcs (funcs) where

-- {{{ Imports

import Tombot.IRC.Types (IRC)
import qualified Tombot.IRC.Types as IRC
import Tombot.Parser
import Tombot.Types
import Tombot.Utils

import Control.Applicative
import Control.Arrow hiding (left, right)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Error
import Control.Exception (SomeException)
import qualified Control.Exception as E
import Control.Lens as Lens hiding (sets)
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Type.Operator

import Combinator.Booly

import qualified Data.Aeson as Aes
import qualified Data.Aeson.Types as Aes
import qualified Data.Aeson.Lens as Aes
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Char (toLower, isLower, isAlphaNum)
import Data.Coerce
import Data.IORef
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid
import Data.Ord (comparing)
import qualified Data.Proxy as Proxy
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as TextErr
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Encoding as TextLazy
import Data.Time (utcToLocalTime, formatTime, defaultTimeLocale
                 , TimeZone(..)
                 )
import Data.Time.Clock.POSIX
import Data.Vector (Vector)
import qualified Data.Vector as Vec

import Debug.Trace

import GHC.Generics

import Network.HTTP (urlDecode, urlEncode)
import qualified Network.Wreq as Wreq

import System.Exit (ExitCode(ExitSuccess))
import System.FilePath.Posix ((</>))
import System.IO (hClose)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomRIO)
import System.Process (readProcessWithExitCode, spawnCommand)

import Text.Regex
import qualified Text.Taggy.Lens as Taggy

-- }}}


-- {{{ funcs

-- | Service-agnostic functions
funcs :: Map Text (Funk s)
funcs = toFunks [ ("!", ddg, Online)
                , ("ban", ban, Mod)
                , ("say", echo, Online)
                , ("kick", kick, Mod)
                , ("voice", voice, Mod)
                , ("currency", exchange, Online)
                , ("find", findMsg, Online)
                , ("food", food, Online)
                , ("lastfm", lastfm, Online)
                , ("in", match, Online)
                , ("manga", manga, Online)
                , ("airing", airing, Online)
                , ("anime", anime, Online)
                , ("random", random, Online)
                , ("remind", remind, Online)
                , ("kickban", kickban, Mod)
                , ("on", respond, Online)
                , ("users", userlist, Online)
                , ("translate", translate, Online)
                , ("ops", ops, Online)
                , ("onfunction", frespond, Online)
                , ("onpriority", prespond, Online)
                , ("onsearch", responds, Online)
                , ("raw", raw, BotOwner)
                , ("sed", sed, Online)
                , ("length", count, Online)
                , ("let", store, Online)
                , ("urban", urbandict, Online)
                , ("bots", bots, Online)
                , ("eval", eval, Online)
                , ("expand", expand, Online)
                , ("help", help, Online)
                , ("history", history, Online)
                , ("http", http, Online)
                , ("isup", isup, Online)
                , ("kill", kill, Online)
                , ("name", name, Online)
                , ("nyaa", nyaa, Online)
                , ("nick", nick, Online)
                , ("quit", quit, Admin)
                , ("stat", stat, Online)
                , ("tell", tell, Online)
                , ("wiki", wiki, Online)
                , ("show", reveal, Online)
                , ("join", chanjoin, Online)
                , ("part", partchan, Mod)
                , ("verbosity", verbosity, Admin)
                , ("cjoin", cjoin, Online)
                , ("every", event, Online)
                , ("fstat", fstat, Mod)
                , ("functions", funcsList, Online)
                , ("kanji", kanji, Online)
                , ("botnicks", nicks, Admin)
                , ("sleep", sleep, Online)
                , ("title", title, Online)
                , ("topic", topic, Online)
                , ("cajoin", cajoin, Online)
                , ("markov", markov, Online)
                , ("prefixes", prefix, Online)
                , ("predict", predict, Online)
                , ("mirror", rwords, Online)
                , ("shadify", shadify, Online)
                , ("version", \_ -> return version, Online)
                , ("unixtime", unixtime, Online)
                , ("formattime", formattime, Online)
                ]

ircFuncs :: Map Text (Funk IRC)
ircFuncs = toFunks [ ("<", priv, Online)
                   , ("me", ircMe, Online)
                   , ("host", host, Online)
                   , ("mode", mode, Mod)
                   ]

-- }}}


-- TODO filters
-- | Low level anime releases function, instead returning a list of strings.
anime' :: Text -> Mind s [String]
anime' str = do
    let (tn, str') = Text.break (== ' ') $ Text.stripStart str
        mn = readMay $ Text.unpack tn :: Maybe Int
        n = maybe 10 id mn
        string = maybe str (const str') mn
    let (matches, filters) = wordbreaks ((== '-') . Text.head) string
        burl = "http://www.nyaa.eu/?page=search&cats=1_37&filter=2&term="
        search = urlEncode . Text.unpack $ Text.unwords matches
        filters' = map (tailSafe . Text.unpack) filters
        url = burl <> search

    ereq <- try $ Wreq.getWith wreqOpts url

    let htmlText = ereq ^. _Right . Wreq.responseBody
                 . Lens.to (TextLazy.decodeUtf8With TextErr.lenientDecode)

    let searchNames = Text.unwords $ htmlText ^.. Taggy.html
            . Taggy.allNamed (only "td")
            . Taggy.attributed (ix "class" . only "tlistname")
            . Taggy.children . Lens.to listToMaybe
            . _Just
            . Taggy.contents

        singleName = htmlText ^.. Taggy.html
            . Taggy.allAttributed (ix "class" . only "viewtorrentname")
            . Taggy.contents

        --animes = maybe sanimes (: sanimes) $ listToMaybe panimes
        --animes' = filter (\x -> not $ any (`isInfixOf` x) filters') animes
    return []
    --return $ take n animes'

-- | Anime releases function.
nyaa :: Text -> Mind s Text
nyaa str = do
    animes <- anime' str
    let animes' = map Text.pack animes
    return $ Text.intercalate ", " animes'


newtype AniToken = AniToken (Int, Text) deriving (Show)

instance Aes.FromJSON AniToken where
    parseJSON (Aes.Object v) =
        fmap AniToken $ (,) <$> v Aes..: "expires"
                            <*> v Aes..: "access_token"
    parseJSON _ = pure $ AniToken (0, "")

data AniAiring =
    AniAiring { airCountdown :: Int
              , airNext_episode :: Int
              , airTime :: Text
              }
              deriving (Show, Generic)
instance Aes.FromJSON AniAiring where
    parseJSON = lowerFromJSON 3

data SmallAniModel =
    SmallAniModel { sanAdult :: Bool
                  , sanAiring :: Maybe AniAiring
                  , sanAiring_status :: Text
                  , sanAverage_score :: Text
                  , sanId :: Int
                  , sanImage_url_lge :: Text
                  , sanImage_url_med :: Text
                  , sanImage_url_sml :: Text
                  , sanPopularity :: Int
                  -- XXX bad ani documentation doesn't specify data type
                  , sanRelation_type :: Aes.Value
                  , sanRole :: Aes.Value
                  , sanSynonyms :: [Text]
                  , sanTitle_english :: Text
                  , sanTitle_japanese :: Text
                  , sanTitle_romaji :: Text
                  , sanTotal_episodes :: Maybe Int
                  , sanType :: Text
                  }
                  deriving (Show, Generic)
instance Aes.FromJSON SmallAniModel where
    parseJSON = lowerFromJSON 3

data AniModel =
    AniModel { anAdult :: Bool
             , anAiring :: Maybe AniAiring
             , anAiring_status :: Text
             , anAverage_score :: Text
             , anClassification :: Maybe Text
             , anDescription :: Text
             , anDuration :: Maybe Int
             , anEnd_date :: Maybe Text
             , anGenres :: [Text]
             , anHashtag :: Maybe Text
             , anId :: Int
             , anImage_url_banner :: Maybe Text
             , anImage_url_lge :: Text
             , anImage_url_med :: Text
             , anImage_url_sml :: Text
             , anList_stats :: Aes.Value
             , anPopularity :: Int
             , anRelation_type :: Aes.Value
             , anRole :: Aes.Value
             , anSource :: Maybe Text
             , anStart_date :: Maybe Text
             , anSynonyms :: [Text]
             , anTitle_english :: Text
             , anTitle_japanese :: Text
             , anTitle_romaji :: Text
             , anTotal_episodes :: Maybe Int
             , anType :: Text
             , anYoutube_id :: Maybe Text
             }
             deriving (Show, Generic)
instance Aes.FromJSON AniModel where
    parseJSON = lowerFromJSON 2

anilistToken :: TMVar (Maybe (Int, Text))
anilistToken = unsafePerformIO $ newTMVarIO Nothing

aniAuthToken :: Mind s (Maybe Text)
aniAuthToken = do
    cid <- maybe mempty id <$> getAPIKey "ani-id"
    cse <- maybe mempty id <$> getAPIKey "ani-secret"
    let args :: [(B.ByteString, B.ByteString)]
        args = [ ("grant_type", "client_credentials")
               , ("client_id", Text.encodeUtf8 cid)
               , ("client_secret", Text.encodeUtf8 cse)
               ]

        url = "https://anilist.co/api/auth/access_token"

    er <- try $ Wreq.post url args

    liftIO $ print er

    let mobj :: Maybe AniToken
        mobj = join $ Aes.decode . (^. Wreq.responseBody) <$> hush er
        mtok :: Maybe (Int, Text)
        mtok = coerce <$> mobj
    liftIO $ putStr "Token received? " >> print mtok
    liftIO $ atomically $ swapTMVar anilistToken mtok
    return $ snd <$> mtok

getToken :: Mind s (Maybe Text)
getToken = do
    mtokpair <- liftIO $ atomically $ readTMVar anilistToken
    timenow <- liftIO $ floor <$> getPOSIXTime

    let mstored = join . flip fmap mtokpair $ \(expires, token) ->
            if timenow < expires then Just token else Nothing
    mtok <- maybe aniAuthToken (return . Just) mstored
    liftIO $ putStr "Token exists? " >> print mstored >> print mtok
    return mtok

anime :: Text -> Mind s Text
anime str = do
    mtok <- getToken

    let mstr = fmap Text.singleton
             . listToMaybe
             . Text.unpack
             $ Text.dropWhile (== ' ') str

    flip (maybe $ return "") (mstr >&> mtok) $ \token -> do
        let opts = wreqOpts
                 & Wreq.header "User-Agent" .~ [Text.encodeUtf8 version]
                 & Wreq.header "Authorization" .~
                     [ "Bearer " <> Text.encodeUtf8 token ]

            encodedStr = urlEncode $ Text.unpack str
            surl = "https://anilist.co/api/anime/search/" <> encodedStr

        liftIO $ print surl

        esr <- try $ Wreq.getWith opts surl

        let baseAurl = "https://anilist.co/api/anime/"
            maurl = esr ^? _Right
                  . Wreq.responseBody
                  . Lens.to (Aes.decode @Aes.Value) . _Just
                  . Aes.nth 0
                  . Aes.key "id"
                  . Lens.to (Aes.parseMaybe Aes.parseJSON)
                  . _Just
                  . Lens.to (show @Int)
                  . Lens.to (baseAurl <>)

        liftIO $ print maurl

        mear <- maybe (pure Nothing) (fmap Just . try . Wreq.getWith opts) maurl

        liftIO $ maybe (return ())
                       (BL.writeFile "anijson" . (^. Wreq.responseBody))
                       (join $ hush <$> mear)

        let mresult :: Maybe Aes.Value
            mresult = mear ^? _Just
                    . _Right
                    . Wreq.responseBody
                    . Lens.to Aes.decode
                    . _Just
            titleRomaji = mresult ^. _Just
                        . Aes.key "title_romaji"
                        . Lens.to (Aes.parseMaybe Aes.parseJSON)
                        . _Just
            titleJapanese = mresult ^. _Just
                          . Aes.key "title_japanese"
                          . Lens.to (Aes.parseMaybe Aes.parseJSON)
                          . _Just
            description = mresult ^. _Just
                        . Aes.key "description"
                        . Lens.to (Aes.parseMaybe Aes.parseJSON)
                        . _Just
            imageUrlMed = mresult ^. _Just
                        . Aes.key "image_url_med"
                        . Lens.to (Aes.parseMaybe Aes.parseJSON)
                        . _Just

            animeText = andMconcat
                      [ titleRomaji
                      , " (" <> titleJapanese <> ")"
                      , ": " <> description 
                      , "\n" <> imageUrlMed
                      ]

        case esr of
            Right _ -> return animeText
            Left _ -> do
                liftIO $ atomically $ swapTMVar anilistToken Nothing
                return ""

-- TODO complete filter, negative searches
airing :: Text -> Mind s Text
airing str = do
    mtok <- getToken

    flip (maybe $ return "") mtok $ \token -> do
        let opts = Wreq.defaults
                 & Wreq.header "User-Agent" .~ [Text.encodeUtf8 version]
                 & Wreq.header "Authorization" .~ [Text.encodeUtf8 token]
                 & Wreq.param "full_page" .~ ["true"]
                 & Wreq.param "airing_data" .~ ["true"]
                 & Wreq.param "status" .~ ["Currently Airing"]
            url = "https://anilist.co/api/browse/anime"

        er <- try $ Wreq.getWith opts url
        tstamp <- liftIO $ getPOSIXTime

        let results :: Aes.Array
            results = er ^. _Right
                    . Wreq.responseBody
                    . Aes._Array
            titleEnglish = results ^.. traverse
                         . Aes.key "title_english"
                         . Lens.to (Aes.parseMaybe Aes.parseJSON)
                         . _Just
            countdown = results ^.. traverse
                      . Aes.key "airing"
                      . Lens.to (Aes.parseMaybe Aes.parseJSON)
                      . (_Just @Aes.Value)
                      . Aes.key "countdown"
                      . Lens.to (Aes.parseMaybe Aes.parseJSON)
                      . (_Just @Int)
            nextEpisode = results ^.. traverse
                        . Aes.key "airing"
                        . Aes.key "next_episode"
                        . Lens.to (Aes.parseMaybe Aes.parseJSON)
                        . (_Just @Int)
            airingTriple = zip3 titleEnglish countdown nextEpisode
            sortedPairs = sortBy (comparing $ view _2) airingTriple
            searchTitles = filter (Text.isInfixOf (Text.toLower str) . Text.toLower . view _1) sortedPairs
            airs = flip map searchTitles $ \(title, time, episode) ->
                let stime = fromIntegral time + tstamp
                    ((days, hours), mins) = over _1 (`divMod` 24)
                                          $ div time 60 `divMod` 60
                    ldays = bool "" (show days <> "d") $ days > 0
                    lhours = bool "" (show hours <> "h") $ hours > 0
                    lmins = bool "" (show mins <> "m") $ mins > 0
                    ldate = Text.pack . unwords $ filter (/= "") [ldays, lhours, lmins]
                in Text.concat [ title, " (ep. ", Text.pack $ show episode
                               , ")", andMconcat [" in ", ldate]
                               ]
            airingText = Text.intercalate ", " $ take 15 airs

        case er of
            Right _ -> return airingText
            Left _ -> do
                liftIO $ atomically $ swapTMVar anilistToken Nothing
                return ""

-- TODO multiple users
--      ban by hostname
-- | Ban a user.
ban :: Text -> Mind s Text
ban str = do
    mode $ "+" <> bs <> " " <> Text.unwords nicks
  where
    nicks = Text.words str
    bs = Text.replicate (length nicks) "b"

-- | Respond to `bots'
bots :: Text -> Mind s Text
bots _ = return "Hi! (λ Haskell)"

-- | Set the channel's ChanAutoJoin value
cajoin :: Text -> Mind s Text
cajoin str = do
    dest <- either _userId _chanName <$> sees _currDestination
    if Text.null $ Text.strip str
    then do
        mchan <- Map.lookup dest . _servChannels . _currServer <$> see
        return $ maybe "" (Text.pack . show . _chanAutoJoin) mchan
    else mwhenPrivileged $ do
        modChanAutoJoin dest $ \aj -> maybe aj id $ readMay $ Text.unpack str
        return ""

-- TODO
-- | Join a channel.
chanjoin :: Text -> Mind s Text
chanjoin str = mvoid . write "IRC" $ "JOIN " <> str

-- | Set the channel's ChanJoin value
cjoin :: Text -> Mind s Text
cjoin str = do
    dest <- either _userId _chanName <$> sees _currDestination
    if Text.null $ Text.strip str
    then do
        mchan <- Map.lookup dest . _servChannels . _currServer <$> see
        return $ maybe "" (Text.pack . show . _chanJoin) mchan
    else mwhenPrivileged $ do
        modChanJoin dest $ \cj -> maybe cj id $ readMay $ Text.unpack str
        return ""

-- XXX should work now, test it
-- | DuckDuckGo !bang search.
ddg :: Text -> Mind s Text
ddg str = do
    let burl = ("https://api.duckduckgo.com/?format=json&q=!" <>)
        opts = Wreq.defaults
             & Wreq.checkStatus .~ (Just $ \_ _ _ -> Nothing)

    er <- try $ Wreq.headWith opts $ burl $ urlEncode $ Text.unpack str

    case er of
        Right r -> liftIO $ print r >> return ""
        Left e -> liftIO $ print e >> return ""

-- | Shadify an URL
shadify :: Text -> Mind s Text
shadify str = do
    let form :: [(B.ByteString, B.ByteString)]
        form = [ ("oourl", Text.encodeUtf8 str)
               , ("category", "shadify")
               ]

    er <- try $ Wreq.post "http://urlify.io/murl.php" form

    flip (either $ \l -> liftIO (print l) >> pure "") er $ \r -> do
        let parser = A.string "<a href='" >> A.takeWhile1 (/= '\'')
            responseText = Text.decodeUtf8
                         . BL.toStrict $ r ^. Wreq.responseBody
            murl = A.parseOnly parser responseText

        either (\_ -> return "") return murl

-- XXX What purpose does this serve now?
--      - Perhaps we can use it for the planned `load' function.
-- | Delete something.
del :: Text -> Mind s Text
del str = return ""

-- TODO
-- | Word definition lookup function.
dict :: Text -> Mind s Text
dict str = do
    return ""

-- | Print the input.
echo :: Text -> Mind s Text
echo = return

-- | Evaluate KawaiiLang.
eval :: Text -> Mind s Text
eval str = do
    fs <- Map.union funcs <$> serverfuncs funcs
    botparse fs str >>= compile fs

-- TODO
-- | Expand KawaiiLang down to base functions and operators.
expand :: Text -> Mind s Text
expand str = do
    fs <- Map.union funcs <$> serverfuncs funcs
    kl <- botparse fs str

    liftIO $ print kl

    return ""

eventThreadsVar :: TVar $ Map Text $ Map (CI Text) $ Map Text ThreadId
{-# NOINLINE eventThreadsVar #-}
eventThreadsVar = unsafePerformIO $ newTVarIO Map.empty

{-
withLocalValue :: Monoid a
               => Map Text $ Map (CI Text) a
               -> (a -> Mind s b)
               -> Mind s $ Map Text $ Map (CI Text) b
withLocalValue m f = do
    serv <- sees $ _servHost . _currServer
    dest <- sees $ either _userId _chanName . _currDestination

    -- Get the value
    let ma = m ^? at serv . _Just . at dest . _Just

    -- Perform the action on the value
    ma' <- f $ maybe mempty id ma

    -- Set the value and return the new Map
    return $ m & at serv . _Just . at dest .~ ma'
-}

-- TODO actually print to channel
-- Load stored threads, check if their threads are active,
-- otherwise fork a thread for them. Add new event too.
-- XXX use mWhenUserStat (>= Admin) when event time argument < 1000
-- | Create a timed event.
event :: Text -> Mind s Text
event str = return "" {-do
    storedThreads <- liftIO $ readTVarIO eventThreadsVar

    -- :: Map Text (POSIXTime, Int -| RepeatTime, Text -| KawaiiLang)
    newMap <- withLocalValue storedThreads $ \threadMap -> do
        -- Add event
        if not $ Text.null $ Text.strip eventFunc then do

            currentTime <- liftIO getPOSIXTime

            tid <- forkMi $ forever $ do
                liftIO $ threadDelay $ maybe 1000 id mayTime
                liftIO . print =<< eval eventFunc

            liftIO $ putStr "Event made " >> print tid

{-
            let mevent = do
                    event <- join $ Map.lookup <$> mayName <*> pure threadMap

                    repeatTime <- mayTime

                    return $ event & _1 ?~ currentTime
                                   & _2 ?~ repeatTime
                                   & _3 ?~ eventFunc
                                   & _4 ?~ tid

-}
            let newMap = Map.insert <$> mayName
                                    <*> pure tid
                                    <*> pure threadMap

            return $ maybe threadMap id newMap

        -- Delete event
        else do
            let mtid :: Maybe ThreadId
                mtid = join $ Map.lookup <$> mayName <*> pure threadMap
                newMap = Map.delete <$> mayName <*> pure threadMap

            -- Kill thread if it exists
            maybe (return ()) (liftIO . killThread) mtid

            return $ maybe threadMap id newMap

    --mvoid $ modLocalStored "events" id
    return ""-}
  where
    !args = Text.words str
    mayName = atMay args 0
    mayTime :: Maybe Int
    mayTime = join
            $ fmap (\n -> bool (const Nothing) Just (n >= 1000) n)
            $ join $ readMay . Text.unpack <$> atMay args 1
    eventFunc = Text.unwords $ drop 2 args

exchangeRateCache = unsafePerformIO $ newEmptyTMVarIO

-- FIXME doesn't work
-- XXX use http://fixer.io/
-- | Currency exchange function.
exchange :: Text -> Mind s Text
exchange str = return ""

-- | Alias for `history' and only returning the message.
findMsg :: Text -> Mind s Text
findMsg str = onlyMsg <$> history str
  where
    onlyMsg = snd . bisect (== '\t') . snd . bisect (== '\t')


data Food2search = Food2search { food2count :: Int
                               , food2recipes :: Vector Food2recipe
                               }
                               deriving (Generic, Show)
instance Aes.FromJSON Food2search where
    parseJSON = lowerFromJSON 5

data Food2recipe = Food2recipe { food2publisher :: Text
                               , food2f2f_url :: Text
                               , food2title :: Text
                               , food2source_url :: Text
                               , food2recipe_id :: Text
                               , food2image_url :: Text
                               , food2social_rank :: Float
                               , food2publisher_url :: Text
                               }
                               deriving (Generic, Show)
instance Aes.FromJSON Food2recipe where
    parseJSON = lowerFromJSON 5

-- | Food2fork random recipe by searching
food :: Text -> Mind s Text
food str = do
    food2key <- maybe "" id <$> getAPIKey "food2fork"

    let opts = Wreq.defaults
             & Wreq.param "key" .~ [food2key]
             & Wreq.param "q" .~ [str]

        surl = "http://food2fork.com/api/search"

    er <- try $ Wreq.getWith opts surl

    -- NOTE The API returns AT MOST 30 results per page
    n <- liftIO $ randomRIO (0, 29)

    let mfood = do
            obj <- join $ Aes.decode . (^. Wreq.responseBody) <$> hush er

            let recipes = food2recipes obj
            recipe <- recipes Vec.!? n

            return $ mconcat [food2title recipe, ": ", food2f2f_url recipe]

    return $ maybe "" id mfood

-- TODO
-- | Function Stat return/change.
fstat :: Text -> Mind s Text
fstat str = return ""

-- TODO nth result argument
-- | Kanji lookup function.
kanji :: Text -> Mind s Text
kanji str = do
    let burl = "http://www.csse.monash.edu.au/~jwb/cgi-bin/wwwjdic.cgi?1ZUR"
        url = burl <> urlEncode (Text.unpack str)
    {- res <- httpGetString url
    let html = fromMaybeElement $ parseXMLDoc res
        qbody = QName "BODY" Nothing Nothing
        body = fromMaybeElement $ findElement qbody html
        text = take 2 . filter (/= "") . lines $ elemsText body
        s = Text.intercalate "; " . map Text.pack $ text
    verb body -}
    return ""

-- | Kick a user.
kick :: Text -> Mind s Text
kick str = do
    edest <- sees _currDestination
    let e = flip fmap edest $ kicks
    either (const $ return "") (>> return "") e
  where
    (nicks', txt) = Text.break (== ':') str
    nicks = Text.words nicks'
    fullkick n c = write "IRC"
                 $ Text.unwords["KICK", CI.original (_chanName c), n, txt]
    kicks c = forM_ nicks $ \n -> fullkick n c

-- | Kick and ban a user.
kickban :: Text -> Mind s Text
kickban str = do
    ban $ Text.takeWhile (/= ':') str
    mvoid $ kick str

-- TODO
-- | Kill an event
kill :: Text -> Mind s Text
kill str = return ""

-- | Count characters.
count :: Text -> Mind s Text
count str = return $ Text.pack . show $ Text.length str

-- FIXME doesn't work
-- TODO store given argument so the user only has to do `:lastfm' in the future
-- XXX move JSON parsing to `json' function once that is finished
-- | Last.fm user get recent track
lastfm :: Text -> Mind s Text
lastfm str = return ""

-- TODO FIXME changing does not work
-- | List the available Funcs.
funcsList :: Text -> Mind s Text
funcsList str = do
    edest <- sees _currDestination
    let dest = either _userId _chanName edest
    if Text.null $ Text.strip str
    then do
        let ea = fmap _chanFuncs edest
            fs = either (const []) Map.keys ea
        return $ Text.unwords fs
    else mwhenPrivileged $ do
        let mv = readMay $ allowedstr :: Maybe (Allowed [Text])
            insert = case mv of
                Just (Whitelist xs)
                    | "funcs" `elem` xs -> const $ Whitelist xs
                    | otherwise -> const $ Whitelist $ "funcs" : xs
                Just (Blacklist xs)
                    | "funcs" `notElem` xs -> const $ Blacklist xs
                    | otherwise -> const $ Blacklist $ filter (/= "funcs") xs
                Nothing -> id
        verb allowedstr
        e <- modChanFuncs dest id -- FIXME
        return $ either id (const "") e
  where
    allowedstr = let (x, y) = show . words . Text.unpack <$> bisect (== ' ') str
                 in unwords [Text.unpack x, y]

-- TODO add more greetings
-- | Greeting from the bot.
greet :: Text -> Mind s Text
greet str = do
    dir <- fmap _confDirectory seeConfig
    mgreets <- readConfig $ dir </> "greet"
    let len = maybe 0 length mgreets
    n <- liftIO $ randomRIO (0, len - 1)
    return $ maybe "" (flip (atDef "") n) mgreets

-- TODO add help for the operators, help syntax and other relevant things
-- | Help for usage of the bot.
help :: Text -> Mind s Text
help str = do
    dir <- fmap _confDirectory seeConfig
    mhelps <- readConfig $ dir </> "help"
    mschans <- readServerStored "letfuncs"
    edest <- sees _currDestination

    let msfuncs = Map.unions . Map.elems <$> mschans
        dest = either _userId _chanName edest
        mlfuncs = join $ Map.lookup dest <$> mschans
        mlsfuncs = Map.union <$> mlfuncs <*> msfuncs

    liftIO $ print mhelps >> print mlsfuncs

    let mboth = mhelps <> mlsfuncs
        mhelp = join $ Map.lookup string <$> mboth

    return $ maybe "" id mhelp
  where
    string = if Text.null $ Text.strip str
             then "help"
             else Text.strip $ Text.toLower str

-- XXX we may be able to use SQL queries to search???
-- | Search the bot's logs and return a matching message if any.
history :: Text -> Mind s Text
history str = do
    let (tn, str') = Text.break (== ' ') $ Text.stripStart str
        mn = readMay $ Text.unpack tn :: Maybe Int
        n = maybe 0 id mn
        string = maybe str (const str') mn
        (matches, filters) = wordbreaks ((== '-') . Text.head) string
        filters' = map (Text.pack . tailSafe . Text.unpack) filters

    edest <- sees _currDestination

    let dest = either _userId _chanName edest

    (rts :: [(Text, Text, Text, Text, Maybe Text)]) <- queryLogs

    let ts = do
            (date, mesg, nick, chnl, miden) <- reverse rts
            let t = date <> "\t" <> nick <> "\t" <> mesg
            guard $ if null matches then True
                    else all (`Text.isInfixOf` t) matches
            guard $ if null filters' then True
                    else not $ any (`Text.isInfixOf` t) filters'
            guard $ CI.original dest == chnl

            return t

        mt = ts `atMay` n

    return $ maybe "" id mt

-- TODO strip non-letters
-- TODO remove a whole line (not chain) on match from that
-- XXX search str in rls, up to strength defined in "s"
markov :: Text -> Mind s Text
markov str = do
    stime <- liftIO $ do
        putStrLn "MARKOVING"
        getPOSIXTime

    let strWords = Vec.fromList $ Text.words str
        args = Vec.drop (Vec.length strWords - strength) strWords
        idiomArgs = Vec.map mkIdiom args

    (rls :: [(Text, Text, Text, Text, Maybe Text)]) <- queryLogs

    let messages = Vec.map (view _2) $ Vec.fromList rls
        indexes v = Vec.iterateN (Vec.length v) (+1) 0

        g chainMap msg =
            let msgIdioms :: Vector Idiom
                msgIdioms = Vec.map mkIdiom $ Vec.fromList $ Text.words msg
                ns :: Vector Int
                ns = indexes msgIdioms
                wordGroups :: Vector $ Vector Idiom
                wordGroups = Vec.scanl (\ws _ -> Vec.drop 1 ws) msgIdioms ns
                noShorts = Vec.takeWhile ((>=(strength*2)) . Vec.length)
                truncate = Vec.map $ Vec.take (strength*2)
                -- FIXME bad variable name
                x = Vec.toList $ truncate $ noShorts $ wordGroups
                wordMap w = Map.singleton (Vec.take strength w) $ Vec.singleton (Vec.drop strength w)
                wordMaps :: [Map (Vector Idiom) $ Vector $ Vector Idiom]
                wordMaps = map wordMap x <> [chainMap]
             in Map.unionsWith (<>) wordMaps
        chains2 :: Map (Vector Idiom) $ Vector $ Vector Idiom
        chains2 = Vec.foldl' g Map.empty messages

    -- Random starting chain
    n2 <- liftIO $ randomRIO (0, Map.size chains2 - 1)
    -- String chains to use, i.e. total length of words * s?
    rlen <- liftIO $ randomRIO (10, 100)

    let initPart :: Vector Idiom
        initPart = maybe (fst $ Map.elemAt n chains2)
                         (const idiomArgs)
                         (Map.lookup idiomArgs chains2)

    generated2 <- chainer2 rlen initPart chains2 mempty

    let generated :: Text
        generated = Vec.foldl' (\acc a -> acc <> origIdiom a <> " ") mempty generated2

    liftIO $ do
        putStr "DONE MARKOVING "
        print =<< fmap (flip (-) stime) getPOSIXTime

    return generated

  where
    -- XXX markov chain strength
    strength = 2
    chainer2 :: vi ~ Vector Idiom
             => Int -> vi -> Map vi $ Vector vi -> vi -> Mind s vi
    chainer2 0 initPart _ acc = pure $ acc <> initPart
    chainer2 n initPart chains acc = do
        let mayTailParts = Map.lookup initPart chains
            newAcc = acc <> initPart

        (flip $ maybe $ pure newAcc) mayTailParts $ \tailParts -> do
            n <- liftIO $ randomRIO (0, Vec.length tailParts - 1)

            let mayRandomTailPart = tailParts Vec.!? n

            (flip $ maybe $ pure newAcc) mayRandomTailPart $ \tailPart ->
                chainer2 (pred n) tailPart chains newAcc


predictOldLogLength :: TVar Int
predictOldLogLength = unsafePerformIO $ newTVarIO 0

predictOldTree :: TVar $ Map Idiom $ Vector $ Map Idiom Int
predictOldTree = unsafePerformIO $ newTVarIO Map.empty

predictOldCorpus :: TVar $ Map Idiom (Int, Int)
predictOldCorpus = unsafePerformIO $ newTVarIO Map.empty

-- XXX use a cache for the predictions (and corpus?)
predict :: Text -> Mind s Text
predict str = do
    (rls :: [(Text, Text, Text, Text, Maybe Text)]) <- liftIO queryLogs

    stime <- liftIO $ do
        putStrLn "PREDICTING"
        getPOSIXTime

    let !newLength = length rls

    oldLength <- liftIO $ atomically $ do
        ol <- readTVar predictOldLogLength
        writeTVar predictOldLogLength newLength

        return ol

    let allMsgs = map (view _2) rls
        -- New messages
    let msgs :: [Text]
        !msgs = take (newLength - oldLength) allMsgs

        vmsgs :: Vector Text
        !vmsgs = Vec.fromList msgs

    oldPT <- liftIO $ readTVarIO predictOldTree

    let pt :: Map Idiom $ Vector $ Map Idiom Int
        pt = appendPredictionTree vmsgs oldPT

    liftIO $ atomically $ writeTVar predictOldTree pt

    let totalMsgs :: Int
        !totalMsgs = Vec.length vmsgs

    oldCorpus <- liftIO $ readTVarIO predictOldCorpus

        -- XXX what do we do with the total count (snd)
    let corpus :: Map Idiom (Int, Int)
        corpus = appendCorpus msgs oldCorpus

    liftIO $ atomically $ writeTVar predictOldCorpus corpus

    let countIdiomMsgs :: Idiom -> Int
        countIdiomMsgs w = maybe 0 fst $ Map.lookup w corpus

        getIdiomSuccessors :: Idiom -> Maybe $ Vector $ Map Idiom Int
        getIdiomSuccessors = flip Map.lookup pt

        lvmws :: [Vector $ Map Idiom Int]
        lvmws = do
            Just mws <- map (getIdiomSuccessors . mkIdiom) $ Text.words str
            return mws

        mergeWith :: Monoid a
                   => (a -> a -> a) -> Vector a -> Vector a -> Vector a
        mergeWith f xs ys =
            let pxs = xs <> Vec.replicate (Vec.length ys - Vec.length xs) mempty
                pys = ys <> Vec.replicate (Vec.length xs - Vec.length ys) mempty
            in Vec.zipWith f pxs pys

        merger :: v ~ (Vector $ Map Idiom Int) => (Int, v) -> v -> v
        merger (i, v) av =
            let f ma mb = Map.intersectionWith (+) ma mb <> ma
            in mergeWith f (Vec.replicate i mempty <> v) av

        merged :: Vector $ Map Idiom Int
        merged = foldr merger mempty $ zip [0 .. ] lvmws

        pickWord mws = do
            let listWords = Map.toList mws
                probs = do
                    (w, prob) <- listWords
                        -- 
                    let tf = fromIntegral prob
                        -- ALL messages
                        n = fromIntegral totalMsgs
                        -- Quantity of messages word appeared within
                        nt = fromIntegral $ countIdiomMsgs w
                        idf = log $ 1 + (n / nt)

                    when (isInfinite nt) $ do
                        traceM (show nt) >> traceM (show idf)

                    when (nt == 0) $ traceM $ show w <> " doesn't exist?"

                    return . ceiling $ tf + idf

            mn <- randomIndexMay probs

            return . fmap fst . join $ atMay listWords <$> mn

        -- XXX verify the newMerged index used in f
        step :: t ~ (Maybe Idiom, Vector $ Map Idiom Int)
             => t -> Int -> Mind s t
        step (Nothing, merged) _ = return (Nothing, merged)
        step (Just oldIdiom, oldMerged) i = do
            let mayNewMerged = do
                    idiomSuccs <- getIdiomSuccessors oldIdiom
                    return $ merger (i, idiomSuccs) oldMerged

                m :: Map Idiom Int
                m = maybe mempty id . join $ fmap (Vec.!? i) mayNewMerged

            mayNewWord <- pickWord m

            return (mayNewWord, maybe oldMerged id mayNewMerged)

    -- XXX verify that predictions are merged with AND
    -- XXX use head of `Text.words str`?
    -- XXX words length is inaccurate
    len <- liftIO $ randomRIO (1, 100)
    let is = [length lvmws .. length lvmws + len]
    prediction <- scanM step (fmap mkIdiom . lastMay $ Text.words str, merged) is

    let lastMerged = maybe merged id $ snd <$> lastMay prediction
        predWords = tailSafe . catMaybes $ map (fmap origIdiom . fst) prediction

    liftIO $ do
        putStr "DONE PREDICTING "
        print =<< fmap (flip (-) stime) getPOSIXTime

    return $ Text.unwords predWords

  where
    randomIndexMay probs = do
        let uniProbs = join $ map (uncurry replicate) $ zip probs [0..]
        n <- liftIO $ randomRIO (0, length uniProbs - 1)
        return $ atMay uniProbs n

-- | The current user's hostname
host :: Text -> Mind IRC Text
host _ = do
  sees $ IRC._userHost . _userService . _currUser

-- | Get a HTTP header from a request.
http :: Text -> Mind s Text
http str = do
    request <- try $ Wreq.getWith wreqOpts $ Text.unpack str

    return ""

-- | Check if a website is up.
isup :: Text -> Mind s Text
isup str = do
    let turl = "http://" <> foldr (flip Text.replace "") str ["https://", "http://"]
        url = Text.unpack turl

    request <- try $ Wreq.getWith wreqOpts url

    let status = request ^? _Right
                          . Wreq.responseStatus
                          . Wreq.statusCode
                          . Lens.to (< 400)

    return $ Text.pack $ maybe "False" show status

-- XXX what will the language look like?
--     CSS inspired language!
--     >, [attr="value"], :nth-child, .class, #id, etc, except adjusted for JSON
-- TODO
json :: Text -> Mind s Text
json str = do
    let mobj = Aes.decode (BL.fromStrict $ Text.encodeUtf8 json) :: Maybe Aes.Value
        v = undefined
    return ""
  where
    (ssel, json) = bisect (== ' ') str

-- TODO filter
-- | Recent manga releases.
manga :: Text -> Mind s Text
manga str = do
    let (tn, str') = Text.break (== ' ') $ Text.stripStart str
        mn = readMay @Int $ Text.unpack tn
        n = maybe 10 id mn
        string = maybe str (const str') mn
        burl = "https://www.mangaupdates.com/releases.html?act=archive&search="
        url = burl <> urlEncode (Text.unpack string)

    ereq <- try $ Wreq.getWith wreqOpts url

    let htmlText = ereq ^. _Right . Wreq.responseBody
                 . Lens.to (TextLazy.decodeUtf8With TextErr.lenientDecode)

        mangaElems = htmlText ^.. Taggy.html
                   . Taggy.allNamed (only "td")
                   . Taggy.attributed (ix "class" . only "text pad")
                   . Taggy.contents

    {- let elem' = fromMaybeElement $ parseXMLDoc html
        qname = QName "td" Nothing Nothing
        attrs = [Attr (QName "class" Nothing Nothing) "text pad"]
        elems = findElementsAttrs qname attrs elem'
        elems' = map elemsText elems -}
        -- Generate a list of colored strings
        genMangas :: [Text] -> [Text]
        genMangas (date:mangatitle:vol:chp:group:rest) =
            let mangaStr = Text.unwords
                    [ "[" <> Text.strip group <> "]"
                    , mangatitle
                    , "[Ch." <> chp <> ","
                    , "Vol." <> vol <> "]"
                    , "(" <> date <> ")"
                    ]
            in mangaStr : genMangas rest
        genMangas _ = []

        mangaLens = scanl' (flip (:)) [] . take n $ genMangas mangaElems
        mangas = takeWhile ((< 400) . Text.length . mconcat) mangaLens

    return $ maybe "" (Text.intercalate ", ") $ lastMay mangas

-- | Show whether the regex matches a string.
match :: Text -> Mind s Text
match str = do
    let c = str `Text.index` 0
        m = A.maybeResult . flip A.feed "" $ A.parse parsematch str
    flip (maybe $ pure "") m $ \(mat, ins, str') -> do
        let regex = mkRegexWithOpts mat False ins
        emins <- try $ return $! matchRegex regex str'
        either (const $ return "") (return . maybe "" (const "True")) emins

-- | Make the bot do an action; also known as /me.
ircMe :: Text -> Mind IRC Text
ircMe str = return $ ctcp $ "ACTION " <> str

-- | Set the channel mode.
mode :: Text -> Mind s Text
mode str = mwhenPrivileged $ do
    edest <- sees _currDestination
    let e = write "IRC" . fullmode <$> edest
    either (const $ return "") (>> return "") e
  where
    (m, s) = Text.break (== ' ') $ Text.strip str
    fullmode c = Text.unwords ["MODE", CI.original $ _chanName c, m, s]

-- | The current user's name
name :: Text -> Mind s Text
name _ = sees $ _userName . _currUser

-- | The current user's nick
nick :: Text -> Mind s Text
nick _ = sees $ _userNick . _currUser

userId :: Text -> Mind s Text
userId _ = sees $ CI.original . _userId . _currUser

-- TODO
-- XXX on erroneous NICK, remove it from the list
-- | Set the bot's nicks
nicks :: Text -> Mind s Text
nicks str = if Text.null $ Text.strip str
    then _botNick . _servBot . _currServer <$> see
    else mwhenUserStat (>= Admin) $ return ""

-- | List the bot's available operators.
ops :: Text -> Mind s Text
ops _ = return "++ -> <- <> >< >> +> == /= => />"

-- | Part from a channel.
partchan :: Text -> Mind s Text
partchan str
    | Text.null $ Text.strip str = do
        edest <- sees _currDestination
        either (const $ pure "") (parter . CI.original . _chanName) edest
    | otherwise = mwhenUserStat (>= Admin) $ parter str
  where
    parter chan = mvoid $ write "IRC" $ "PART " <> chan

-- | Set the channel's prefix `Char's
prefix :: Text -> Mind s Text
prefix str = do
    dest <- either _userId _chanName <$> sees _currDestination
    if Text.null $ Text.strip str
    then do
        mchan <- Map.lookup dest . _servChannels . _currServer <$> see
        return $ maybe "" (Text.pack . _chanPrefix) mchan
    else mwhenPrivileged $ do
        mvoid . modChan dest $ Lens.set chanPrefix $ Text.unpack str

-- | Send a private message to the user.
priv :: Text -> Mind s Text
priv str = do
    nick <- sees $ CI.original . _userId . _currUser
    mvoid $ putPrivmsg nick str

-- | Disconnect from an IRC server.
quit :: Text -> Mind s Text
quit str = return ""

-- | Pick a random choice or number.
random :: Text -> Mind s Text
random str
    | isDigits $ Text.strip str = do
        let mi :: Maybe Integer
            mi = maybeRead $ Text.unpack str
        flip (maybe $ return mempty) mi $ \i -> do
            n <- liftIO $ randomRIO (0, i)
            return . Text.pack $ show n
    | otherwise = do
        let choices = Text.split (== '|') str
            len = length choices
        n <- liftIO $ randomRIO (0, len - 1)
        return $ maybe "" id $ choices `atMay` n
  where
    isDigits = Text.all (`elem` ['0' .. '9'])
    maybeRead = fmap fst . listToMaybe . reads

-- | Write directly to the IRC handle of the current server.
raw :: Text -> Mind s Text
raw str = mvoid $ write "IRC" str

-- TODO
-- FIXME this explodes
-- | Reload the bot's Config and Funcs.
reload :: Text -> Mind s Text
reload _ = return ""

-- | Remind a user on join.
remind :: Text -> Mind s Text
remind str = do
    cnick <- sees $ _userNick . _currUser
    let f :: Maybe Text -> Maybe Text
        f = maybe (Just msg) $ if nick == cnick
                               then if Text.null $ Text.strip msg
                                    then const Nothing
                                    else const $ Just msg
                               else Just
    mvoid . modLocalStored "remind" $ Map.alter f nick
  where
    (nick, msg) = bisect (== ' ') str

-- | Respond to a regex match by running a function with any match(es) as the
--   argument.
--
-- > :on /http:\/\/\S+/ :title \0
-- > :on /what should i do/i :ra |Nothing!|Do your work!|Stop procrastinating!
respond :: Text -> Mind s Text
respond str = do
    dir <- fmap _confDirectory seeConfig
    let m = A.maybeResult . flip A.feed "" $ A.parse parsematch str
    flip (maybe $ pure "") m $ \(mat, ins, str') -> do
        let (ns, str'') = fmap Text.unpack . bisect (== ' ') $ Text.pack str'
            n :: Int
            (n, string) = maybe (10, str') (,str'') $ readMay $ Text.unpack ns
            f = if null (dropWhile (== ' ') str')
                then Nothing
                else Just (ins, n, string)

        liftIO $ print mat >> print ins >> print str'

        mvoid . modLocalStored "respond" $ Map.alter (const f) mat

-- FIXME prioritize locals
responds :: Text -> Mind s Text
responds str = do
    (msons :: Maybe (Map Text _)) <- readServerStored "respond"
    let mons = Map.unions . Map.elems <$> msons
    let ons :: [(String, (Bool, Int, String))]
        ons = sortBy (comparing $ snd3 . snd) $ maybe mempty Map.toList mons

    liftIO $ print $ map fst ons

    ons' <- flip filterM ons $ \(match, (ins, n, resp)) -> deci . decide $ do
        let regex = mkRegexWithOpts match False ins
        emins <- try $ return $! matchRegex regex $ Text.unpack str
        verb ("onMatch: " <> show emins)
        when (isLeft emins) $ do
            throwError False
        let mins = either (const $ Just []) id emins
        unless (isJust mins) $ throwError False
        return True

    let matchOns = map (wrap . fst) ons'
        allOns = map (wrap . fst) ons
    return . Text.pack . intercalate ", " $ matchOns <|< allOns
  where
    replace a b c = Text.unpack $ Text.replace (Text.pack a) (Text.pack b) (Text.pack c)
    deci :: Mind s (Either Bool Bool) -> Mind s Bool
    deci m = m >>= return . either id id
    wrap t = andMconcat ["/", t, "/"]

frespond :: Text -> Mind s Text
frespond str = do
    (msons :: Maybe (Map Text _)) <- readServerStored "respond"
    let mons = Map.unions . Map.elems <$> msons
    let mon :: Maybe (Bool, Int, Text)
        mon = join $ Map.lookup key <$> mons
        on = maybe "" (view _3) mon
    return on
  where
    key = Text.init . Text.tail $ str

prespond :: Text -> Mind s Text
prespond str = do
    (msons :: Maybe (Map Text _)) <- readServerStored "respond"
    let mons = Map.unions . Map.elems <$> msons
    let mon :: Maybe (Bool, Int, Text)
        mon = join $ Map.lookup key <$> mons
        on = maybe "" (Text.pack . show . view _2) mon
    return on
  where
    key = Text.init . Text.tail $ str

-- | Show StateT data.
reveal :: Text -> Mind s Text
reveal str = do
    case double of
        ("Config", _) -> do
            Text.pack . show <$> sees _currConfig
        ("User", nick) -> return ""
            --getUser (CI.mk nick) >>= return . maybe "" (Text.pack . show)
        ("Chan", chan) -> return ""
            --getChan chan >>= return . maybe "" (Text.pack . show)
        _ -> return ""
  where
    double = first Text.strip . second Text.strip $ bisect (== ' ') str

-- | Reverse word order.
rwords :: Text -> Mind s Text
rwords = return . Text.unwords . reverse . Text.words

-- | Regex replace function.
--
-- > .sed s\/apples\/Google\/i I love apples!
sed :: Text -> Mind s Text
sed str = mwhen (Text.length str > 1) $ do
    let c = str `Text.index` 1
        m = A.maybeResult . flip A.feed "" $ A.parse parsesed str
    flip (maybe $ pure "") m $ \(mat, rep, ins, str') -> do
        let regex = mkRegexWithOpts mat False ins
        e <- liftIO $ try (pure $! subRegex regex str' rep)
        either (const $ pure "") (pure . Text.pack) e

-- | Set a User's Stat
stat :: Text -> Mind s Text
stat str = do
    let mv = readMay $ Text.unpack value :: Maybe UserStatus
    if Text.null $ Text.strip value
    then do
        users <- sees $ _servUsers . _currServer
        return $ maybe "" (Text.pack . show . _userStatus) $ Map.lookup nick users
    else do
        mwhenUserStat (>= Admin) $ do
            servhost <- _servHost <$> sees _currServer
            dir <- _confDirectory <$> seeConfig
            mservs <- readConfig $ dir </> "UserStats"
            let f = maybe (Just . Map.delete nick) ((Just .) . Map.insert nick) mv
                mservs' = Map.alter (join . fmap f) servhost <$> mservs
                servs = maybe mempty id mservs'
            writeConf (dir </> "UserStats") servs
            e <- modUser nick $ over userStatus (\s -> maybe s id mv)
            return $ either id (const "") e
  where
    (nick, value) = first CI.mk $ bisect (== ' ') str

-- | Delay a function by n seconds where n is a floating point number.
sleep :: Text -> Mind s Text
sleep str = do
    let mn = readMay (Text.unpack $ Text.strip str) :: Maybe Double
    mvoid $ case mn of
        Just n -> liftIO $ threadDelay $ fromEnum $ 10^6*n
        _ -> return ()

-- | `store' adds a new func to the Map and runs the contents through `eval'.
store :: Text -> Mind s Text
store str = do
    let isFunc = Map.member name funcs

    mlfuncs <- (readLocalStored @(Map Text Text) "letfuncs")

    liftIO $ print $ maybe (-1) Map.size mlfuncs

    let lfuncs = maybe mempty id mlfuncs
        inserter f = if Text.null $ Text.strip func
                     then Map.delete name
                     else Map.insert name f

    mvoid . when (not isFunc) $ do
        modLocalStored "letfuncs" $ inserter func
  where
    (name, func) = first Text.toLower $ bisect (== ' ') str

-- | Store a message for a user that is printed when they talk next.
tell :: Text -> Mind s Text
tell str = do
    serv <- sees $ _servHost . _currServer
    edest <- sees $ _currDestination
    cnick <- sees $ _userNick . _currUser
    if Text.null $ Text.strip msg'
    then do
        musers <- readLocalStored "tell"
        let texts = maybe [] id $ join $ Map.lookup nick <$> musers
        return . maybe "" id . fst $ pipeJoin texts
    else mvoid . modLocalStored "tell" $
            let f = Just . maybe [msg cnick] (++ [msg cnick])
            in Map.alter f nick
  where
    (nick, msg') = bisect (== ' ') str
    msg c = Text.take 400 msg' <> " (from " <> c <> ")"

unixtime :: Text -> Mind s Text
unixtime str = Text.pack . show . floor <$> liftIO getPOSIXTime

formattime :: Text -> Mind s Text
formattime  str = do
    let utct = posixSecondsToUTCTime . fromIntegral $ read tstamp
        offset = if maybe False (== '-') $ headMay tzone
                    then readDef 0 tzone
                    else readDef 0 $ dropWhile (=='+') tzone
        loct = utcToLocalTime (TimeZone offset False "JST") utct
        locale = defaultTimeLocale
    pure . Text.pack $ formatTime locale format loct
  where
    (tstamp, str') = over _1 Text.unpack $ bisect (== ' ') str
    (tzone, format) = over both Text.unpack $ bisect (== ' ') str'

-- | Website title fetching function.
title :: Text -> Mind s Text
title str = do
    (con, hed, _) <- httpGetResponse (Text.unpack str)
    let respType = maybe "" id $ lookup "Content-Type" hed
    mwhen ("text/html" `isInfixOf` map toLower respType) $ do
        let mtitle = headMay $ TextLazy.pack con ^.. Taggy.html
                . Taggy.allNamed (only "title")
                . Taggy.contents

        return $ maybe "" Text.strip mtitle

-- TODO
-- | Channel topic changing function.
topic :: Text -> Mind s Text
topic str
    | Text.null $ Text.strip str = do
        e <- sees _currDestination
        flip (either $ const $ pure "") e $ return . _chanTopic
    | otherwise = mwhenPrivileged $ do
        edest <- sees _currDestination
        let e = flip fmap edest $ \c -> do
            let t = modder $ _chanTopic c
            unless (t == _chanTopic c) $ do
                let chanText = CI.original $ _chanName c
                write "IRC" $ "TOPIC " <> chanText <> " :" <> t
                write "IRC" $ "TOPIC " <> chanText
            return t
        either (const $ return "") (>> return "") e
  where
    modder
        | Text.null $ Text.strip str = id
        | Text.head str == '+' = flip mappend $ Text.tail str
        | Text.head str == '-' = removeLast (Text.tail str)
        | otherwise = const str
    -- TODO move to Utils
    removeLast :: Text -> Text -> Text
    removeLast t ts =
        let len = Text.length t
            t' = Text.reverse t
            ts' = Text.reverse ts
            (beg, end) = Text.drop len <$> Text.breakOn t' ts'
        in Text.reverse $ beg <> end

-- | Google translate.
translate :: Text -> Mind s Text
translate str = do
    let (tl', str') = bisect (== ' ') str
        (tl, string) = if Text.length tl' == 2 && Text.all isLower tl'
                       then (Text.unpack tl', Text.unpack str')
                       else ("en", Text.unpack str)
    !m <- googleTranslate "auto" tl string
    let mtrans = (map $ maybeToMonoid . headMay) . fst <$> m
    return . Text.concat $ maybe [] id mtrans

-- | Urban dictionary lookup function.
urbandict :: Text -> Mind s Text
urbandict str = do
    let burl = "http://api.urbandictionary.com/v0/define?term="

    request <- try $ Wreq.getWith wreqOpts $ burl ++ Text.unpack str

    let jsonLBS = request ^. _Right . Wreq.responseBody
        mjson :: Maybe Aes.Value
        mjson = Aes.decode jsonLBS
        mdefinition = mjson ^? _Just
                             . Aes.key "list"
                             . Aes.nth 0
                             . Aes.key "definition"
                             . Lens.to (Aes.parseMaybe Aes.parseJSON)
                             . _Just

    (flip $ maybe $ return "") mdefinition $ \definition -> do
        return $ str <> ": " <> Text.replace "\n" " | " definition

-- | Print the channel's userlist.
userlist :: forall s. Text -> Mind s Text
userlist _ = do
    edest <- sees _currDestination
    users <- sees $ Map.elems . _servUsers . _currServer

    return $ either _userNick (chanNicks users) edest
  where
    chanNicks :: [User s] -> Channel s -> Text
    chanNicks us c = Text.unwords
        $ map _userNick
        $ filter ((/= Offline) . _userStatus)
        $ filter (Set.member (_chanName c) . _userChannels) us

-- | Set the Config verbosity
verbosity :: Text -> Mind s Text
verbosity str = do
    if Text.null $ Text.strip str
    then Text.pack . show . _confVerbosity <$> seeConfig
    else mwhenUserStat (== BotOwner) $ do
        mapConfig $ over confVerbosity $ \v ->
            maybe v id $ readMay $ Text.unpack str

        return ""

-- | Give voice to users.
voice :: Text -> Mind s Text
voice str = mode $ "+" <> vs <> " " <> Text.unwords nicks
  where
    nicks = Text.words str
    vs = Text.replicate (length nicks) "v"


data WikiPage = WikiPage { wikipPageid :: !Int
                         , wikipNs :: !Int
                         , wikipTitle :: !Text
                         , wikipExtract :: !Text
                         }
                         deriving (Generic, Show)
instance Aes.FromJSON WikiPage where
    parseJSON = lowerFromJSON 5

data WikiQuery = WikiQuery { wikiqNormalized :: Maybe Aes.Value
                           , wikiqRedirects :: Maybe Aes.Value
                           , wikiqPages :: Map Text WikiPage
                           }
                           deriving (Show, Generic)
instance Aes.FromJSON WikiQuery where
    parseJSON = lowerFromJSON 5

data WikiBase = WikiBase { wikiBatchcomplete :: !Text
                         , wikiQuery :: !WikiQuery
                         }
                         deriving (Show, Generic)
instance Aes.FromJSON WikiBase where
    parseJSON = lowerFromJSON 4

-- | Wikipedia summary fetching.
wiki :: Text -> Mind s Text
wiki str = do
    let (lang', str') = bisect (== ' ') str
        (lang, queryStr) = if isLang lang' && not (Text.null $ Text.strip str')
                        then (lang', str')
                        else ("en", str)

        url = concat ["https://"
                     , Text.unpack lang
                     , ".wikipedia.org/w/api.php"
                     , "?format=json&action=query&prop=extracts"
                     , "&exintro=&explaintext=&redirects=1&titles="
                     , urlEncode $ Text.unpack queryStr
                     ]

        opts = Wreq.defaults
             & Wreq.header "User-Agent" .~ [Text.encodeUtf8 version]

    !ereq <- try $ Wreq.getWith opts url

    when (isLeft ereq) $ liftIO $ print ereq

    let extract = ereq ^. _Right . Wreq.responseBody
                . Lens.to (join . Aes.decode)
                . _Just
                . Lens.to (Map.maxView . wikiqPages . wikiQuery)
                . _Just . _1
                . Lens.to (Text.take 1000 . wikipExtract)

    return extract

  where
    isLang x = Text.length x == 2 || x == "simple"

