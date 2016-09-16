
-- This file is part of Tombot, licensed under the GNU GPL 2 license.
-- See the file "LICENSE" for more information.
-- Copyright Shou, 2013

{-# LANGUAGE OverloadedStrings, DoAndIfThenElse, BangPatterns,
             TupleSections, ScopedTypeVariables, DeriveGeneric,
             PartialTypeSignatures, TypeOperators, TypeFamilies #-}

module Tombot.Funcs (funcs) where

-- {{{ Imports

import Tombot.Types
import Tombot.Utils
import Tombot.Parser

import Control.Applicative
import Control.Arrow hiding (left, right)
import Control.BoolLike
import Control.Concurrent
import Control.Concurrent.STM
import Control.Error
import Control.Exception (SomeException)
import qualified Control.Exception as E
import Control.Lens as L hiding (sets)
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Either (left, right)
import Control.Type.Operator

import Data.Aeson
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.CaseInsensitive as CI
import Data.Char (toLower, isLower, isAlphaNum)
import Data.Coerce
import Data.IORef
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Time (utcToLocalTime, formatTime, defaultTimeLocale
                 , TimeZone(..)
                 )
import Data.Time.Clock.POSIX
import Data.Vector (Vector)
import qualified Data.Vector as V

import Debug.Trace

import qualified Language.Haskell.Interpreter as H

import GHC.Generics

import Network.HTTP (urlDecode, urlEncode)
import Network.Wreq ( getWith, post, postWith, headWith, responseBody
                    , defaults, param, header, FormParam(..)
                    , checkStatus
                    )

import System.Exit (ExitCode(ExitSuccess))
import System.IO (hClose)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomRIO)
import System.Process (readProcessWithExitCode, spawnCommand)

import qualified Text.JSON as J
import Text.Regex
import Text.XML.Light

-- }}}


funcs :: Map Text Funk
funcs = toFunks [ ("!", ddg, Online)
                , ("b", ban, Mod)
                , ("say", echo, Online)
                , ("<", priv, Online)
                , ("kick", kick, Mod)
                , ("mode", mode, Mod)
                , ("voice", voice, Mod)
                , ("currency", exchange, Online)
                , ("find", findMsg, Online)
                , ("food", food, Online)
                , ("me", me, Online)
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
                , ("help", help, Online)
                , ("history", history, Online)
                , ("host", host, Online)
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
                , ("verb", verbosity, Admin)
                , ("cjoin", cjoin, Online)
                , ("every", event, Online)
                , ("fstat", fstat, Mod)
                , ("functions", list, Online)
                , ("kanji", kanji, Online)
                , ("botnicks", nicks, Admin)
                , ("sleep", sleep, Online)
                , ("title", title, Online)
                , ("topic", topic, Online)
                , ("cajoin", cajoin, Online)
                , ("markov", markov, Online)
                , ("botprefixes", prefix, Online)
                , ("romaji", romaji, Online)
                , ("connect", connectIRC, Admin)
                , ("predict", predict, Online)
                , ("mirror", rwords, Online)
                , ("restart", restart, BotOwner)
                , ("shadify", shadify, Online)
                , ("version", \_ -> return version, Online)
                , ("unixtime", unixtime, Online)
                , ("formattime", formattime, Online)
                ]


-- TODO
about :: Func
about _ = return ""

-- TODO filters
-- | Low level anime releases function, instead returning a list of strings.
anime' :: Text -> Mind [String]
anime' str = do
    let (tn, str') = T.break (== ' ') $ T.stripStart str
        mn = readMay $ T.unpack tn :: Maybe Int
        n = maybe 10 id mn
        string = maybe str (const str') mn
    let (matches, filters) = wordbreaks ((== '-') . T.head) string
        burl = "http://www.nyaa.eu/?page=search&cats=1_37&filter=2&term="
        search = urlEncode . T.unpack $ T.unwords matches
        filters' = map (tailSafe . T.unpack) filters
    html <- liftIO $ httpGetString $ burl <> search
    let elem' = fromMaybeElement $ parseXMLDoc html
        qname = QName "td" Nothing Nothing
        sresults = findElementsAttrs qname [className "tlistname"] elem'
        presults = findElementsAttrs qname [className "viewtorrentname"] elem'
        sanimes = map (elemsText) sresults
        panimes = map (elemsText) presults
        animes = maybe sanimes (: sanimes) $ listToMaybe panimes
        animes' = filter (\x -> not $ any (`isInfixOf` x) filters') animes
    return $ take n animes'
  where
    className = Attr (QName "class" Nothing Nothing)

-- | Anime releases function.
nyaa :: Func
nyaa str = do
    animes <- anime' str
    let animes' = map T.pack animes
    return $ T.intercalate ", " animes'


newtype AniToken = AniToken (Int, Text) deriving (Show)

instance FromJSON AniToken where
    parseJSON (Object v) = fmap AniToken $ (,) <$> v .: "expires"
                                               <*> v .: "access_token"
    parseJSON _ = pure $ AniToken (0, "")

data AniAiring =
    AniAiring { airCountdown :: Int
              , airNext_episode :: Int
              , airTime :: Text
              }
              deriving (Show, Generic)
instance FromJSON AniAiring where
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
                  , sanRelation_type :: Value
                  , sanRole :: Value
                  , sanSynonyms :: [Text]
                  , sanTitle_english :: Text
                  , sanTitle_japanese :: Text
                  , sanTitle_romaji :: Text
                  , sanTotal_episodes :: Maybe Int
                  , sanType :: Text
                  }
                  deriving (Show, Generic)
instance FromJSON SmallAniModel where
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
             , anList_stats :: Value
             , anPopularity :: Int
             , anRelation_type :: Value
             , anRole :: Value
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
instance FromJSON AniModel where
    parseJSON = lowerFromJSON 2

anilistToken :: TMVar (Maybe (Int, Text))
anilistToken = unsafePerformIO $ newTMVarIO Nothing

aniAuthToken :: Mind (Maybe Text)
aniAuthToken = do
    cid <- maybe mempty id <$> getAPIKey "ani-id"
    cse <- maybe mempty id <$> getAPIKey "ani-secret"
    let args :: [(B.ByteString, B.ByteString)]
        args = [ ("grant_type", "client_credentials")
               , ("client_id", T.encodeUtf8 cid)
               , ("client_secret", T.encodeUtf8 cse)
               ]
        url = "https://anilist.co/api/auth/access_token"
    er <- try $ post url args
    let mobj :: Maybe AniToken
        mobj = join $ decode . (^. responseBody) <$> hush er
        mtok :: Maybe (Int, Text)
        mtok = coerce <$> mobj
    liftIO $ putStr "Token received? " >> print mtok
    liftIO $ atomically $ swapTMVar anilistToken mtok
    return $ snd <$> mtok

getToken :: Mind (Maybe Text)
getToken = do
    mtokpair <- liftIO $ atomically $ readTMVar anilistToken
    timenow <- liftIO $ floor <$> getPOSIXTime

    let mstored = join . flip fmap mtokpair $ \(expires, token) ->
            if timenow < expires then Just token else Nothing
    mtok <- maybe aniAuthToken (return . Just) mstored
    liftIO $ putStr "Token exists? " >> print mstored >> print mtok
    return mtok

anime :: Func
anime str = do
    mtok <- getToken

    let mstr = fmap T.singleton
             . listToMaybe
             . T.unpack
             $ T.dropWhile (== ' ') str

    flip (maybe $ return "") (mstr >&> mtok) $ \token -> do
        let opts = defaults
                 & header "User-Agent" .~ [T.encodeUtf8 version]
                 & header "Authorization" .~ [T.encodeUtf8 token]
            encodedStr = urlEncode $ T.unpack str
            surl = "https://anilist.co/api/anime/search/" <> encodedStr

        liftIO $ print surl

        esr <- try $ getWith opts surl
        let manimeId = join $ do
                -- obj :: Maybe [SmallAniModel]
                obj <- join $ decode . (^. responseBody) <$> hush esr
                return $ show . sanId <$> headMay obj
            maurl = ("https://anilist.co/api/anime/" <>) <$> manimeId

        liftIO $ print maurl

        mear <- maybe (pure Nothing) (fmap Just . try . getWith opts) maurl

        liftIO $ maybe (return ())
                       (BL.writeFile "anijson" . (^. responseBody))
                       (join $ hush <$> mear)

        let result = maybe "" id $ do
                -- obj :: Maybe AniModel
                obj <- join $ decode . (^. responseBody) <$> join (fmap hush mear)
                return $ T.concat [ anTitle_romaji obj
                                  , " (" <> anTitle_japanese obj <> ")"
                                  , ": " <> anDescription obj
                                  , "\n" <> anImage_url_med obj
                                  ]

        case esr of
            Right _ -> return result
            Left _ -> do
                liftIO $ atomically $ swapTMVar anilistToken Nothing
                return ""

-- TODO complete filter, negative searches
airing :: Func
airing str = do
    mtok <- getToken

    flip (maybe $ return "") mtok $ \token -> do
        let opts = defaults
                 & header "User-Agent" .~ [T.encodeUtf8 version]
                 & header "Authorization" .~ [T.encodeUtf8 token]
                 & param "full_page" .~ ["true"]
                 & param "airing_data" .~ ["true"]
                 & param "status" .~ ["Currently Airing"]
            url = "https://anilist.co/api/browse/anime"

        er <- try $ getWith opts url
        tstamp <- liftIO $ getPOSIXTime

        let mpairs = do
                -- obj :: [SmallAniModel]
                obj <- join $ decode . (^. responseBody) <$> hush er
                -- not actually a pair lol
                return $ zipWith3 (,,)
                    (map sanTitle_english obj)
                    (map (fmap airCountdown . sanAiring) obj)
                    (map (fmap airNext_episode . sanAiring) obj)
            pairs = join $ maybeToList mpairs
            justAiring = map (over _3 fromJust)
                       $ map (over _2 $ maybe 0 id)
                       $ filter (isJust . view _3) pairs
            sortedPairs = sortBy (comparing $ view _2) justAiring
            searchTitles = filter (T.isInfixOf (T.toLower str) . T.toLower . view _1) sortedPairs
            airs = flip map searchTitles $ \(title, time, episode) ->
                let stime = fromIntegral time + tstamp
                    ((days, hours), mins) = over _1 (`divMod` 24)
                                          $ div time 60 `divMod` 60
                    ldays = bool "" (show days <> "d") $ days > 0
                    lhours = bool "" (show hours <> "h") $ hours > 0
                    lmins = bool "" (show mins <> "m") $ mins > 0
                    ldate = T.pack . unwords $ filter (/= "") [ldays, lhours, lmins]
                in T.concat [ title, " (ep. ", T.pack $ show episode
                            , ")", andmconcat [" in ", ldate]
                            ]
            airingText = T.intercalate ", " $ take 15 airs

        case er of
            Right _ -> return airingText
            Left _ -> do
                liftIO $ atomically $ swapTMVar anilistToken Nothing
                return ""

-- TODO multiple users
--      ban by hostname
-- | Ban a user.
ban :: Func
ban str = do
    mode $ "+" <> bs <> " " <> T.unwords nicks
  where
    nicks = T.words str
    bs = T.replicate (length nicks) "b"

-- | Respond to `bots'
bots :: Func
bots _ = return "Hi! (λ Haskell)"

-- | Set the channel's ChanAutoJoin value
cajoin :: Func
cajoin str = do
    dest <- either origNick stChanName <$> sees currDest
    if T.null $ T.strip str
    then do
        mchan <- M.lookup dest . stServChans . currServ <$> see
        return $ maybe "" (T.pack . show . stChanAutoJoin) mchan
    else mwhenPrivileged $ do
        modChan dest $ \c -> c { stChanAutoJoin = let v = stChanAutoJoin c
                                                      mv = readMay $ T.unpack str
                                                  in maybe v id mv
                               }
        return ""

-- | Join a channel.
chanjoin :: Func
chanjoin str = mvoid . write $ "JOIN " <> str

-- | Set the channel's ChanJoin value
cjoin :: Func
cjoin str = do
    dest <- either origNick stChanName <$> sees currDest
    if T.null $ T.strip str
    then do
        mchan <- M.lookup dest . stServChans . currServ <$> see
        return $ maybe "" (T.pack . show . stChanJoin) mchan
    else mwhenPrivileged $ do
        modChan dest $ \c -> c { stChanJoin = let v = stChanJoin c
                                                  mv = readMay $ T.unpack str
                                              in maybe v id mv
                               }
        return ""

-- FIXME !bang my ANUS
-- XXX should work now, test it
-- | DuckDuckGo !bang search.
ddg :: Func
ddg str = do
    let burl = ("https://api.duckduckgo.com/?format=json&q=!" <>)
        opts = L.set checkStatus (Just $ \_ _ _ -> Nothing) defaults

    er <- try $ headWith opts $ burl $ urlEncode $ T.unpack str

    case er of
        Right r -> liftIO $ print r >> return ""
        Left e -> liftIO $ print e >> return ""

-- | Shadify an URL
shadify :: Func
shadify str = do
    let form :: [(B.ByteString, B.ByteString)]
        form = [ ("oourl", T.encodeUtf8 str)
               , ("category", "shadify")
               ]
    er <- try $ post "http://urlify.io/murl.php" form
    flip (either $ \l -> liftIO (print l) >> pure "") er $ \r -> do
        let parser = A.string "<a href='" >> A.takeWhile1 (/= '\'')
            responseText = T.decodeUtf8 . BL.toStrict $ r ^. responseBody
            murl = A.parseOnly parser responseText

        either (\_ -> return "") return murl

-- XXX What purpose does this serve now?
--      - Perhaps we can use it for the planned `load' function.
-- | Delete something.
del :: Func
del str = return ""

-- TODO
-- | Word definition lookup function.
dict :: Func
dict str = do
    return ""

-- | Print the input.
echo :: Func
echo = return

-- | Evaluate KawaiiLang.
eval :: Func
eval str = do
    -- XXX very spooky??? i.e. does this allfunc magic work?
    fs <- serverfuncs
    botparse fs str >>= compile fs

-- XXX use mWhenUserStat (>= Admin) when event time argument < 1000
-- TODO time argument
-- | Create an event. Useful with `sleep'.
event :: Func
event str = do
    d <- either origNick stChanName . currDest <$> see
    kill name
    (if time < 1000 then mwhenUserStat (>= Admin) else id) $ do
        tid <- forkMi $ whileAlive $ do
            liftIO $ threadDelay $ fromEnum $ 10^6*time
            t <- eval skl
            -- TODO FIXME send
            --putPrivmsg d t
            liftIO $ print t
        sets $ \c ->
            c { currServ =
                (currServ c) { stServThreads =
                    let tds = stServThreads $ currServ c
                    in M.insert name tid tds }}
        return $ "Made event: " <> name
  where
    (name, str') = bisect (== ' ') str
    (tim, skl) = bisect (== ' ') str'
    time = readDef 1000 (T.unpack tim) :: Int
    whileAlive m = do
        evs <- sees $ stServThreads . currServ
        if M.member name evs
            then m >> whileAlive m
            else void $ kill name

exchangeRateCache = unsafePerformIO $ newEmptyTMVarIO

-- FIXME doesn't work
-- XXX use http://fixer.io/
-- | Currency exchange function.
exchange :: Func
exchange str = do
    let url = [ "http://rate-exchange.appspot.com/currency?from=" <> T.unpack a
              , "&to=" <> T.unpack b
              ]
    jsonStr <- httpGetString $ concat url
    let mres :: Maybe (J.JSObject J.JSValue)
        mres = resultMay $ J.decode jsonStr
        mdubz :: Maybe Float
        mdubz = do
            o <- mres
            join . fmap floatMay . lookup "rate" $ J.fromJSObject o
    return $ maybe "" (T.pack . show . (x *)) mdubz
  where
    (x, str') = first (read . T.unpack) $ bisect (== ' ') str
    (a, b) = bisect (== ' ') str'

-- | Alias for `history' and only returning the message.
findMsg :: Func
findMsg str = onlyMsg <$> history str
  where
    onlyMsg = snd . bisect (== '\t') . snd . bisect (== '\t')


data Food2search = Food2search { food2count :: Int
                               , food2recipes :: Vector Food2recipe
                               }
                               deriving (Generic, Show)
instance FromJSON Food2search where
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
instance FromJSON Food2recipe where
    parseJSON = lowerFromJSON 5

-- | Food2fork random recipe by searching
food :: Func
food str = do
    food2key <- maybe "" id <$> getAPIKey "food2fork"

    let opts = defaults
             & param "key" .~ [food2key]
             & param "q" .~ [str]
        surl = "http://food2fork.com/api/search"

    er <- try $ getWith opts surl

    -- NOTE The API returns AT MOST 30 results per page
    n <- liftIO $ randomRIO (0, 29)

    let mfood = do
            obj <- join $ decode . (^. responseBody) <$> hush er

            let recipes = food2recipes obj
            recipe <- recipes V.!? n

            return $ mconcat [ food2title recipe
                             , ": "
                             , food2f2f_url recipe
                             ]

    return $ maybe "" id mfood

-- TODO
-- | Function Stat return/change.
fstat :: Func
fstat str = do
    mfunc <- M.lookup tfun . stConfFuncs <$> readConfig
    mmaybe mfunc $ \f -> do
        let stat = funkStat f
        if T.null tsta
        then do
            return . T.pack $ show stat
        else mmaybe mstat $ \s -> do
            let f' = f { funkStat = s }
            mwhenPrivTrans stat $ mwhenPrivTrans s $ mapConfig $ \c ->
                c { stConfFuncs = M.insert tfun f' $ stConfFuncs c }
            return ""
  where
    (tfun, tsta) = bisect (== ' ') str
    mstat = readMay $ T.unpack tsta
    mmaybe = flip $ maybe $ return mempty

-- TODO nth result argument
-- | Kanji lookup function.
kanji :: Func
kanji str = do
    let burl = "http://www.csse.monash.edu.au/~jwb/cgi-bin/wwwjdic.cgi?1ZUR"
        url = burl <> urlEncode (T.unpack str)
    res <- httpGetString url
    let html = fromMaybeElement $ parseXMLDoc res
        qbody = QName "BODY" Nothing Nothing
        body = fromMaybeElement $ findElement qbody html
        text = take 2 . filter (/= "") . lines $ elemsText body
        s = T.intercalate "; " . map T.pack $ text
    verb body
    return s

-- | Kick a user.
kick :: Func
kick str = do
    edest <- sees $ currDest
    let e = flip fmap edest $ kicks
    either (const $ return "") (>> return "") e
  where
    (nicks', txt) = T.break (== ':') str
    nicks = T.words nicks'
    fullkick n c = write $ "KICK " <> stChanName c <> " " <> n <> " " <> txt
    kicks c = forM_ nicks $ \n -> fullkick n c

-- | Kick and ban a user.
kickban :: Func
kickban str = do
    ban $ T.takeWhile (/= ':') str
    mvoid $ kick str

-- | Kill an event
kill :: Func
kill str = do
    evs <- sees $ stServThreads . currServ
    let mev = M.lookup str evs
    flip (maybe $ return "") mev $ \ev -> do
        liftIO $ killThread ev
        let evs' = M.delete str evs
        sets $ \c -> c { currServ = (currServ c) { stServThreads = evs' } }
        return $ "Killed event: " <> str

-- TODO
-- | Connect to an IRC server.
connectIRC :: Func
connectIRC str = do
    mtc <- M.lookup host . stConfServs <$> readConfig
    -- Check if Curr is Disconnected, Connect if so.
    -- But if Nothing then make a new Server and connect. Fork a new thread.
    case mtc of
        Just tc -> return ()
        Nothing -> do
            c <- get
            cs <- sees currServ
            let bns = stServBotNicks cs
                bna = stServBotName cs
                s = defStServ { stServHost = host
                              , stServPort = port
                              , stServBotNicks = bns
                              , stServBotName = bna
                              }
            --initialise c s
            return ()
    return ""
  where
    (host, sport) = first T.unpack $ bisect (== ' ') str
    port = fromIntegral . readDef 6667 $ T.unpack sport

-- | Count characters.
count :: Func
count str = return $ T.pack . show $ T.length str

-- TODO store given argument so the user only has to do `:lastfm' in the future
-- XXX move JSON parsing to `json' function once that is finished
-- | Last.fm user get recent track
lastfm :: Func
lastfm str = do
    mkey <- getAPIKey "lastfm"
    verb mkey
    let url = [ "http://ws.audioscrobbler.com/2.0/"
              , "?method=user.getrecenttracks"
              , "&user=" <> T.unpack str
              , "&api_key=" <> T.unpack (maybe "" id mkey)
              , "&limit=1&extended=1&format=json"
              ]
    jsonStr <- httpGetString $ concat url
    let mres :: Maybe (J.JSObject J.JSValue)
        mres = resultMay $ J.decode jsonStr
        -- TODO split this up, if Nothing on any it returns Nothing
        morcs = do
            o <- mres
            jorcs <- lookup "recenttracks" $ J.fromJSObject o
            J.fromJSObject <$> objectMay jorcs
        user = do
            orcs <- morcs
            jours <- lookup "@attr" orcs
            ours <- J.fromJSObject <$> objectMay jours
            jsuser <- lookup "user" ours
            J.fromJSString <$> stringMay jsuser
        mtr = do
            orcs <- morcs
            trs <- join . fmap arrayMay $ lookup "track" orcs
            fmap J.fromJSObject . join . fmap objectMay $ trs `atMay` 0
        bnp = maybe False id $ do
            tr <- mtr
            jars <- lookup "@attr" tr
            ars <- J.fromJSObject <$> objectMay jars
            sb <- join . fmap stringMay $ lookup "nowplaying" ars
            return $ J.fromJSString sb == "true"
        np = if bnp then " ▸" else ""
        artist = maybe "" id $ do
            tr <- mtr
            jart <- lookup "artist" tr
            art <- J.fromJSObject <$> objectMay jart
            jartist <- lookup "name" art
            J.fromJSString <$> stringMay jartist
        blove = maybe False id $ do
            tr <- mtr
            jlove <- lookup "loved" tr
            (== "1") . J.fromJSString <$> stringMay jlove
        love = if blove then "\ETX13♥\ETX" else "\ETX10♫\ETX"
        title = maybe "" id $ do
            tr <- mtr
            jtitle <- lookup "name" tr
            J.fromJSString <$> stringMay jtitle

    verb mtr
    verb blove

    mwhen (length title > 0) $ do
        return . T.pack $ unwords [ love <> np, title, "\ETX06●\ETX", artist ]
  where
    uni x = read "\"" <> x <> "\""

-- | List the available Funcs.
list :: Func
list str = do
    funcs <- stConfFuncs <$> readConfig
    edest <- sees currDest
    let dest = either origNick stChanName edest
    if T.null $ T.strip str
    then do
        let ea = fmap stChanFuncs edest
            ef = flip fmap ea $ allow (M.keys funcs \\) id
            fs = either (const []) id ef
        return $ T.unwords fs
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
        e <- modChanFuncs dest $ insert
        return $ either id (const "") e
  where
    allowedstr = let (x, y) = show . words . T.unpack <$> bisect (== ' ') str
                 in unwords [T.unpack x, y]

-- TODO specify server/channel
-- | Global message.
glob :: Func
glob str = do
    tcs <- M.elems . stConfServs <$> readConfig
    forM_ tcs $ \tc -> void . liftIO . forkIO . void . flip runStateT tc $ do
        chans <- M.keys . stServChans . currServ <$> see
        forM_ chans $ \chan -> do
            putPrivmsg chan str
            liftIO $ do
                n <- randomRIO (3, 6)
                threadDelay (10^6 * n)
    return ""

-- TODO add more greetings
-- | Greeting from the bot.
greet :: Func
greet str = do
    dir <- fmap stConfDir readConfig
    mgreets <- readConf $ dir <> "greet"
    let len = maybe 0 length mgreets
    n <- liftIO $ randomRIO (0, len - 1)
    return $ maybe "" (flip (atDef "") n) mgreets

-- TODO add help for the operators, help syntax and other relevant things
-- | Help for usage of the bot.
help :: Func
help str = do
    dir <- fmap stConfDir readConfig
    mhelps <- readConf $ dir <> "help"
    mschans <- readServerStored "letfuncs"
    edest <- sees currDest
    let msfuncs = M.unions . M.elems <$> mschans
        dest = either userName stChanName edest
        mlfuncs = join $ M.lookup dest <$> mschans
        mlsfuncs = M.union <$> mlfuncs <*> msfuncs
    liftIO $ print mhelps >> print mlsfuncs
    let mboth = mhelps <> mlsfuncs
    let mhelp = join $ M.lookup string <$> mboth
    return $ maybe "" id mhelp
  where
    string = if T.null $ T.strip str
             then "help"
             else T.strip $ T.toLower str

-- XXX we may be able to use SQL queries to search???
-- | Search the bot's logs and return a matching message if any.
history :: Func
history str = do
    let (tn, str') = T.break (== ' ') $ T.stripStart str
        mn = readMay $ T.unpack tn :: Maybe Int
        n = maybe 0 id mn
        string = maybe str (const str') mn
        (matches, filters) = wordbreaks ((== '-') . T.head) string
        filters' = map (T.pack . tailSafe . T.unpack) filters

    edest <- sees currDest
    let dest = either origNick stChanName edest

    (rts :: [(Text, Text, Text, Text, Maybe Text)]) <- queryLogs
    let ts = do
            (date, mesg, nick, chnl, miden) <- reverse rts
            let t = date <> "\t" <> nick <> "\t" <> mesg
            guard $ if null matches then True
                    else all (`T.isInfixOf` t) matches
            guard $ if null filters' then True
                    else not $ any (`T.isInfixOf` t) filters'
            guard $ dest == chnl

            return t

        mt = ts `atMay` n

    return $ maybe "" id mt

-- TODO strip non-letters
-- TODO remove a whole line (not chain) on match from that
-- XXX search str in rls, up to strength defined in "s"
markov :: Func
markov str = do
    let args = V.take s . V.fromList $ T.words str
    (rls :: [(Text, Text, Text, Text, Maybe Text)]) <- queryLogs

    let mesgs = V.map (view _2) $ V.fromList rls
        -- Generate all s length word combinations of a message
        f :: Text -> Vector $ Vector Text
        f t = let ws = V.fromList $ T.words t
                  ns = V.iterateN (V.length ws) (+1) 0
                  noShorts = V.takeWhile ((>=(s*2)) . V.length)
                  wordGroups = V.scanl (\b _ -> V.drop 1 b) ws ns
              in V.map (V.take (s*2)) . noShorts $ wordGroups
        chains :: Vector $ Vector Text
        chains = join $ V.map f mesgs

    -- Random starting chain
    n <- liftIO $ randomRIO (0, V.length chains - 1)
    -- String chains to use, i.e. total length of words * s?
    rlen <- liftIO $ randomRIO (10, 50)

    -- Use user supplied string if it isn't empty or just whitespace
    let mchain = Just args >|> ((args <|<) <$> chains `V.indexM` n)

    generated <- T.unwords . V.toList . join <$> maybe (pure mempty) (chainer rlen chains) mchain

    liftIO $ do
        putStrLn ""
        print (take 5 rls)
        print (V.take 5 chains)
        print n >> print rlen >> print mchain
        putStrLn ""

    return generated

  where
    s = 2 -- XXX markov chain strength
    matcher :: vt ~ Vector Text => vt -> Vector vt -> Vector vt
    matcher c cs = do
        cm <- cs
        guard $ not (V.null c || V.null cm)
        let ciAlphaNums = V.map (CI.mk . T.filter isAlphaNum)
        guard $ ciAlphaNums (V.drop s c) == ciAlphaNums (V.take s cm)
        return cm
    chainer :: vt ~ Vector Text => Int -> Vector vt -> vt -> Mind $ Vector vt
    chainer 0 cs c = pure $ V.singleton c
    chainer n cs c = do
        let mcs = matcher c cs
        liftIO $ print (V.length mcs)
        n <- liftIO $ randomRIO (0, V.length mcs - 1)
        let mc = mcs `V.indexM` n
        maybe (pure $ V.singleton c) (\jc -> (V.take s c `V.cons`) <$> chainer (n-1) cs jc) mc

-- XXX use a cache for the predictions (and corpus?)
predict :: Func
predict str = do
    (rls :: [(Text, Text, Text, Text, Maybe Text)]) <- liftIO queryLogs

    stime <- liftIO $ do
        putStrLn "PREDICTING"
        getPOSIXTime

    let msgs :: [Text]
        !msgs = map (view _2) rls

        vmsgs :: Vector Text
        !vmsgs = V.fromList msgs

        pt :: Map Idiom $ Vector $ Map Idiom Int
        pt = makePredictionTree vmsgs

        totalMsgs :: Int
        !totalMsgs = V.length vmsgs

        -- XXX what do we do with the total count (snd)
        corpus :: Map Idiom (Int, Int)
        corpus = makeCorpus msgs

        countIdiomMsgs :: Idiom -> Int
        countIdiomMsgs w = maybe 0 fst $ M.lookup w corpus

        getIdiomSuccessors :: Idiom -> Maybe $ Vector $ Map Idiom Int
        getIdiomSuccessors = flip M.lookup pt

        lvmws :: [Vector $ Map Idiom Int]
        lvmws = do
            Just mws <- map (getIdiomSuccessors . mkIdiom) $ T.words str
            return mws

        mergeWith :: Monoid a
                   => (a -> a -> a) -> Vector a -> Vector a -> Vector a
        mergeWith f xs ys =
            let pxs = xs <> V.replicate (V.length ys - V.length xs) mempty
                pys = ys <> V.replicate (V.length xs - V.length ys) mempty
            in V.zipWith f pxs pys

        merger :: v ~ (Vector $ Map Idiom Int) => (Int, v) -> v -> v
        merger (i, v) av =
            let f ma mb = M.intersectionWith (+) ma mb <> ma
            in mergeWith f (V.replicate i mempty <> v) av

        merged :: Vector $ Map Idiom Int
        merged = foldr merger mempty $ zip [0 .. ] lvmws

        pickWord mws = do
            let listWords = M.toList mws
                probs = do
                    (w, prob) <- listWords
                    let tf = fromIntegral prob
                        n = fromIntegral totalMsgs
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
             => t -> Int -> Mind t
        step (Nothing, merged) _ = return (Nothing, merged)
        step (Just oldIdiom, oldMerged) i = do
            let mayNewMerged = do
                    idiomSuccs <- getIdiomSuccessors oldIdiom
                    return $ merger (i, idiomSuccs) oldMerged

                m :: Map Idiom Int
                m = maybe mempty id . join $ fmap (V.!? i) mayNewMerged

            mayNewWord <- pickWord m

            return (mayNewWord, maybe oldMerged id mayNewMerged)

    -- XXX verify that predictions are merged with AND
    -- XXX use head of `T.words str`?
    -- XXX words length is inaccurate
    len <- liftIO $ randomRIO (1, 100)
    let is = [length lvmws .. length lvmws + len]
    prediction <- scanM step (fmap mkIdiom . lastMay $ T.words str, merged) is

    let lastMerged = maybe merged id $ snd <$> lastMay prediction
        predWords = tailSafe . catMaybes $ map (fmap origIdiom . fst) prediction

    liftIO $ do
        putStr "DONE PREDICTING "
        print =<< fmap (flip (-) stime) getPOSIXTime

    return $ T.unwords predWords

  where
    randomIndexMay probs = do
        let uniProbs = join $ map (uncurry replicate) $ zip probs [0..]
        n <- liftIO $ randomRIO (0, length uniProbs - 1)
        return $ atMay uniProbs n

-- | The current user's hostname
host :: Func
host _ = sees $ userHost . currUser

-- | Get a HTTP header from a request.
http :: Func
http str = do
    hed <- httpHead $ T.unpack httpURL
    return $ maybe "" T.pack $ M.lookup (CI.mk $ T.unpack httpType) hed
  where
    (httpType, httpURL) = bisect (== ' ') str

-- FIXME is this sufficient?
-- | Check if a website is up.
isup :: Func
isup str = do
    let url = "http://" <> foldr (flip T.replace "") str ["https://", "http://"]
    (_, _, status) <- httpGetResponse (T.unpack url)
    return $ if isPrefixOf "2" status then "True" else "False"

-- XXX what will the language look like?
--     CSS inspired language!
--     >, [attr="value"], :nth-child, .class, #id, etc, except adjusted for JSON
-- TODO
json :: Func
json str = do
    let mobj = decode (BL.fromStrict $ T.encodeUtf8 json) :: Maybe Value
        v = undefined
    return ""
  where
    (ssel, json) = bisect (== ' ') str

-- TODO filter
-- | Recent manga releases.
manga :: Func
manga str = do
    let (tn, str') = T.break (== ' ') $ T.stripStart str
        mn = readMay $ T.unpack tn :: Maybe Int
        n = maybe 10 id mn
        string = maybe str (const str') mn
        burl = "https://www.mangaupdates.com/releases.html?act=archive&search="
        url = burl <> urlEncode (T.unpack string)
    html <- httpGetString url
    let elem' = fromMaybeElement $ parseXMLDoc html
        qname = QName "td" Nothing Nothing
        attrs = [Attr (QName "class" Nothing Nothing) "text pad"]
        elems = findElementsAttrs qname attrs elem'
        elems' = map elemsText elems
        -- Generate a list of colored strings
        genMangas :: [String] -> [String]
        genMangas (date:mangatitle:vol:chp:group:rest) =
            let mangaStr = unwords [ "\ETX10[" ++ strip group ++ "]\ETX"
                                   , mangatitle
                                   , "[Ch.\ETX06" ++ chp ++ "\ETX,"
                                   , "Vol.\ETX06" ++ vol ++ "\ETX]"
                                   , "(\ETX10" ++ date ++ "\ETX)"
                                   ]
            in mangaStr : genMangas rest
        genMangas _ = []
        mangas = take n $ genMangas elems'
    return . T.pack $ joinUntil ((>= 400) . length) ", " mangas
  where
    strip = T.unpack . T.strip . T.pack

-- | Show whether the regex matches a string.
match :: Func
match str = do
    let c = str `T.index` 0
        m = A.maybeResult . flip A.feed "" $ A.parse parsematch str
    flip (maybe $ pure "") m $ \(mat, ins, str') -> do
        let regex = mkRegexWithOpts mat False ins
        emins <- try $ return $! matchRegex regex str'
        either (const $ return "") (return . maybe "" (const "True")) emins

-- | Make the bot do an action; also known as /me.
me :: Func
me str = return $ ctcp $ "ACTION " <> str

-- | Set the channel mode.
mode :: Func
mode str = mwhenPrivileged $ do
    edest <- sees $ currDest
    let e = flip fmap edest $ write . fullmode
    either (const $ return "") (>> return "") e
  where
    (m, s) = T.break (== ' ') $ T.strip str
    fullmode c = "MODE " <> stChanName c <> " " <> m <> " " <> s

mueval :: Func
mueval str = do
    let file = "Start.hs"
        args = [ "-t", "5", "-n", "-i", "-l", file, "-XOverloadedStrings"
               , "-XTupleSections", "-XDoAndIfThenElse", "-XBangPatterns"
               , "-e", T.unpack str'
               ]
    (xc, sto, ste) <- liftIO $ readProcessWithExitCode "mueval" args ""
    let out = if length (lines sto) == 3
              then lines sto !! 2
              else intercalate "; " $ lines sto
        outType = if length (lines sto) == 3
                  then lines sto !! 1
                  else intercalate "; " $ lines sto
        result = if isTypeOf then outType else out
    return . T.pack $ result
  where
    isTypeOf = T.isPrefixOf ":t" str
    str' = if isTypeOf then T.drop 2 str else str

-- | The current user's name
name :: Func
name _ = sees $ userName . currUser

-- | The current user's nick
nick :: Func
nick _ = sees $ origNick . currUser

-- TODO
-- XXX on erroneous NICK, remove it from the list
-- | Set the bot's nicks
nicks :: Func
nicks str = if T.null $ T.strip str
    then T.unwords . stServBotNicks . currServ <$> see
    else mwhenUserStat (>= Admin) $ return ""

-- | List the bot's available operators.
ops :: Func
ops _ = return "++ -> <- <> >< >> +> == /= => />"

-- | Part from a channel.
partchan :: Func
partchan str
    | T.null $ T.strip str = do
        edest <- sees currDest
        either (const $ pure "") (parter . stChanName) edest
    | otherwise = mwhenUserStat (>= Admin) $ parter str
  where
    parter chan = mvoid $ write $ "PART " <> chan

-- | Set the channel's prefix `Char's
prefix :: Func
prefix str = do
    dest <- either origNick stChanName <$> sees currDest
    if T.null $ T.strip str
    then do
        dest <- either origNick stChanName <$> sees currDest
        mchan <- M.lookup dest . stServChans . currServ <$> see
        return $ maybe "" (T.pack . stChanPrefix) mchan
    else mwhenPrivileged $ do
        mvoid . modChan dest $ \c -> c { stChanPrefix = T.unpack str }

-- | Send a private message to the user.
priv :: Func
priv str = do
    nick <- sees $ origNick . currUser
    mvoid $ putPrivmsg nick str

-- | Disconnect from an IRC server.
quit :: Func
quit str = do
    mtc <- M.lookup (T.unpack str) . stConfServs <$> readConfig
    flip (maybe $ return "") mtc $ \tc -> do
        mvoid . liftIO . flip runStateT tc $ do
            h <- sees currHandle
            tid <- sees currThreadId
            write $ "QUIT :Good bye :c"
            liftIO $ do
                killThread tid
                hClose h

-- | Pick a random choice or number.
random :: Func
random str
    | isDigits $ T.strip str = do
        let mi :: Maybe Integer
            mi = maybeRead $ T.unpack str
        flip (maybe $ return mempty) mi $ \i -> do
            n <- liftIO $ randomRIO (0, i)
            return . T.pack $ show n
    | otherwise = do
        let choices = T.split (== '|') str
            len = length choices
        n <- liftIO $ randomRIO (0, len - 1)
        return $ maybe "" id $ choices `atMay` n
  where
    isDigits = T.all (`elem` ['0' .. '9'])
    maybeRead = fmap fst . listToMaybe . reads

-- | Write directly to the IRC handle of the current server.
raw :: Func
raw str = mvoid $ write str

-- TODO
-- FIXME this explodes
-- | Reload the bot's Config and Funcs.
reload :: Func
reload _ = do
    confpath <- stConfPath <$> readConfig
    ec <- loadModules [confpath] ["Config"] "config" (H.as :: Config)
    let e = flip fmap ec $ \config -> do
        verb $ confVerbosity config
        verb $ confLogging config
        verb $ confPath config
        --verb $ map fst $ M.toList $ confFuncs config
        --mapConfig $ const (toStConf config)
        return ""
    either (\x -> warn x >> return "") id e
  where
    loadModules ps ms x as = liftIO $ H.runInterpreter $ do
        H.loadModules ps
        H.setTopLevelModules ms
        H.interpret x as

-- | Remind a user on join.
remind :: Func
remind str = do
    cnick <- sees $ origNick . currUser
    let f :: Maybe Text -> Maybe Text
        f = maybe (Just msg) $ if nick == cnick
                               then if T.null $ T.strip msg
                                    then const Nothing
                                    else const $ Just msg
                               else Just
    mvoid . modLocalStored "remind" $ M.alter f nick
  where
    (nick, msg) = bisect (== ' ') str

-- | Respond to a regex match by running a function with any match(es) as the
--   argument.
--
-- > :on /http:\/\/\S+/ :title \0
-- > :on /what should i do/i :ra |Nothing!|Do your work!|Stop procrastinating!
respond :: Func
respond str = do
    dir <- fmap stConfDir readConfig
    let m = A.maybeResult . flip A.feed "" $ A.parse parsematch str
    flip (maybe $ pure "") m $ \(mat, ins, str') -> do
        let (ns, str'') = fmap T.unpack . bisect (== ' ') $ T.pack str'
            n :: Int
            (n, string) = maybe (10, str') (,str'') $ readMay $ T.unpack ns
            f = if null (dropWhile (== ' ') str')
                then Nothing
                else Just (ins, n, string)

        liftIO $ print mat >> print ins >> print str'

        mvoid . modLocalStored "respond" $ M.alter (const f) mat

-- FIXME prioritize locals
responds :: Func
responds str = do
    (msons :: Maybe (Map Text _)) <- readServerStored "respond"
    let mons = M.unions . M.elems <$> msons
    let ons :: [(String, (Bool, Int, String))]
        ons = sortBy (comparing $ snd3 . snd) $ maybe mempty M.toList mons
    liftIO $ print $ map fst ons
    ons' <- flip filterM ons $ \(match, (ins, n, resp)) -> deci . decide $ do
        let regex = mkRegexWithOpts match False ins
        emins <- try $ return $! matchRegex regex $ T.unpack str
        verb ("onMatch: " <> show emins)
        when (isLeft emins) $ do
            left False
        let mins = either (const $ Just []) id emins
        unless (isJust mins) $ left False
        right True
    let matchOns = map (wrap . fst) ons'
        allOns = map (wrap . fst) ons
    return . T.pack . intercalate ", " $ matchOns <|< allOns
  where
    replace a b c = T.unpack $ T.replace (T.pack a) (T.pack b) (T.pack c)
    deci :: Mind (Either Bool Bool) -> Mind Bool
    deci m = m >>= return . either id id
    wrap t = andmconcat ["/", t, "/"]

frespond :: Func
frespond str = do
    (msons :: Maybe (Map Text _)) <- readServerStored "respond"
    let mons = M.unions . M.elems <$> msons
    let mon :: Maybe (Bool, Int, Text)
        mon = join $ M.lookup key <$> mons
        on = maybe "" (view _3) mon
    return on
  where
    key = T.init . T.tail $ str

prespond :: Func
prespond str = do
    (msons :: Maybe (Map Text _)) <- readServerStored "respond"
    let mons = M.unions . M.elems <$> msons
    let mon :: Maybe (Bool, Int, Text)
        mon = join $ M.lookup key <$> mons
        on = maybe "" (T.pack . show . view _2) mon
    return on
  where
    key = T.init . T.tail $ str

-- FIXME restarting takes a while, is probably because of STM
-- | Restart the bot.
restart :: Func
restart _ = do
    tmvars <- M.elems . stConfServs <$> readConfig
    hs <- fmap (map (id $!)) . forM tmvars $ \t -> do
        verb "Reading TMVar..."
        c <- liftIO (atomically $ readTMVar t)
        verb "TMVar read."
        return $! currHandle c
    forM_ hs (void . liftIO . try . hClose)
    liftIO $ do
        spawnCommand "tombot"
    return ""

-- | Show StateT data.
reveal :: Func
reveal str = do
    case double of
        ("Config", _) -> do
            t <- sees currConfigTMVar
            v <- liftIO . atomically $ readTMVar t
            return . T.pack . show $ v
        ("User", nick) ->
            getUser (CI.mk nick) >>= return . maybe "" (T.pack . show)
        ("Chan", chan) -> getChan chan >>= return . maybe "" (T.pack . show)
        _ -> return ""
  where
    double = first T.strip . second T.strip $ bisect (== ' ') str

-- | Kana/kanji to romaji function
romaji :: Func
romaji str = do
    m <- gtranslate "ja" "en" $ T.unpack str
    let mtrans = (map $ maybeToMonoid . flip atMay 3) . fst <$> m
    return . T.unwords $ maybe [] id mtrans

-- | Reverse word order.
rwords :: Func
rwords = return . T.unwords . reverse . T.words

-- | Regex replace function.
--
-- > .sed s\/apples\/Google\/i I love apples!
sed :: Func
sed str = mwhen (T.length str > 1) $ do
    let c = str `T.index` 1
        m = A.maybeResult . flip A.feed "" $ A.parse parsesed str
    flip (maybe $ pure "") m $ \(mat, rep, ins, str') -> do
        let regex = mkRegexWithOpts mat False ins
        e <- liftIO $ try (pure $! subRegex regex str' rep)
        either (const $ pure "") (pure . T.pack) e

-- | Set a User's Stat
stat :: Func
stat str = do
    let mv = readMay $ T.unpack value :: Maybe UserStatus
    if T.null $ T.strip value
    then do
        users <- sees $ stServUsers . currServ
        return $ maybe "" (T.pack . show . userStat) $ M.lookup nick users
    else do
        mwhenUserStat (>= Admin) $ do
            servhost <- stServHost <$> sees currServ
            dir <- stConfDir <$> readConfig
            mservs <- readConf $ dir <> "UserStats"
            let f = maybe (Just . M.delete nick) ((Just .) . M.insert nick) mv
                mservs' = M.alter (join . fmap f) servhost <$> mservs
                servs = maybe mempty id mservs'
            writeConf (dir <> "UserStats") servs
            e <- modUser nick $ \u ->
                let stat = userStat u
                in u { userStat = maybe stat id mv }
            return $ either id (const "") e
  where
    (nick, value) = first CI.mk $ bisect (== ' ') str

-- | Delay a function by n seconds where n is a floating point number.
sleep :: Func
sleep str = do
    let mn = readMay (T.unpack $ T.strip str) :: Maybe Double
    mvoid $ case mn of
        Just n -> liftIO $ threadDelay $ fromEnum $ 10^6*n
        _ -> return ()

-- | `store' adds a new func to the Map and runs the contents through `eval'.
store :: Func
store str = do
    dir <- fmap stConfDir readConfig
    funcs <- stConfFuncs <$> readConfig
    let f = eval . (func <>)
    isFunc <- M.member name . stConfFuncs <$> readConfig
    mlfuncs <- readLocalStored "letfuncs" :: Mind (Maybe (Map Text Text))
    let lfuncs = maybe mempty id mlfuncs
        isStored = M.member name lfuncs
        inserter :: a -> Map Text a -> Map Text a
        inserter f = if T.null $ T.strip func
                     then M.delete name
                     else M.insert name f
    mvoid . when (isStored || not isFunc) $ do
        mapConfig $ \c -> c {
            stConfFuncs = inserter (Funk func f Online) $ stConfFuncs c
        }
        mvoid $ modLocalStored "letfuncs" $ inserter func
  where
    (name, func) = first T.toLower $ bisect (== ' ') str

-- | Store a message for a user that is printed when they talk next.
tell :: Func
tell str = do
    serv <- sees $ stServHost . currServ
    edest <- sees $ currDest
    cnick <- sees $ origNick . currUser
    if T.null $ T.strip msg'
    then do
        musers <- readLocalStored "tell"
        let texts = maybe [] id $ join $ M.lookup nick <$> musers
        return . maybe "" id . fst $ pipeJoin texts
    else mvoid . modLocalStored "tell" $
            let f = Just . maybe [msg cnick] (++ [msg cnick])
            in M.alter f nick
  where
    (nick, msg') = bisect (== ' ') str
    msg c = T.take 400 msg' <> " (from " <> c <> ")"

unixtime :: Func
unixtime str = T.pack . show . floor <$> liftIO getPOSIXTime

formattime :: Func
formattime  str = do
    let utct = posixSecondsToUTCTime . fromIntegral $ read tstamp
        offset = if maybe False (== '-') $ headMay tzone
                    then readDef 0 tzone
                    else readDef 0 $ dropWhile (=='+') tzone
        loct = utcToLocalTime (TimeZone offset False "JST") utct
        locale = defaultTimeLocale
    pure . T.pack $ formatTime locale format loct
  where
    (tstamp, str') = over _1 T.unpack $ bisect (== ' ') str
    (tzone, format) = over both T.unpack $ bisect (== ' ') str'

-- | Website title fetching function.
title :: Func
title str = do
    (con, hed, _) <- httpGetResponse (T.unpack str)
    let respType = maybe "" id $ lookup "Content-Type" hed
    mwhen ("text/html" `isInfixOf` map toLower respType) $ do
        let xml = fromMaybeElement $ parseXMLDoc con
            qTitle = QName "title" Nothing Nothing
            elem = fromMaybeElement $ findElementAttrs qTitle [] xml
            text = elemsText elem
        return $ T.strip $ T.pack text

-- TODO
-- | Channel topic changing function.
topic :: Func
topic str
    | T.null $ T.strip str = do
        e <- sees currDest
        flip (either $ const $ pure "") e $ \c -> return $ stChanTopic c
    | otherwise = mwhenPrivileged $ do
        edest <- sees currDest
        let e = flip fmap edest $ \c -> do
            let t = modder $ stChanTopic c
            unless (t == stChanTopic c) $ do
                write $ "TOPIC " <> stChanName c <> " :" <> t
                write $ "TOPIC " <> stChanName c
            return t
        either (const $ return "") (>> return "") e
  where
    modder
        | T.null $ T.strip str = id
        | T.head str == '+' = flip mappend $ T.tail str
        | T.head str == '-' = removeLast (T.tail str)
        | otherwise = const str
    -- TODO move to Utils
    removeLast :: Text -> Text -> Text
    removeLast t ts =
        let len = T.length t
            t' = T.reverse t
            ts' = T.reverse ts
            (beg, end) = T.drop len <$> T.breakOn t' ts'
        in T.reverse $ beg <> end

-- | Google translate.
translate :: Func
translate str = do
    let (tl', str') = bisect (== ' ') str
        (tl, string) = if T.length tl' == 2 && T.all isLower tl'
                       then (T.unpack tl', T.unpack str')
                       else ("en", T.unpack str)
    !m <- gtranslate "auto" tl string
    let mtrans = (map $ maybeToMonoid . headMay) . fst <$> m
    return . T.concat $ maybe [] id mtrans

-- | Urban dictionary lookup function.
urbandict :: Func
urbandict str = do
    let burl = "http://api.urbandictionary.com/v0/define?term="
    jsonStr <- httpGetString (burl ++ T.unpack str)
    let mres :: Maybe (J.JSObject J.JSValue)
        mres = resultMay $ J.decode jsonStr
        def = maybe "" id $ do
            o <- mres
            jlist <- lookup "list" $ J.fromJSObject o
            jarr <- join . fmap (`atMay` 0) $ arrayMay jlist
            ar <- J.fromJSObject <$> objectMay jarr
            jdef <- lookup "definition" ar
            J.fromJSString <$> stringMay jdef
    mwhen (not $ null def) $ do
        return $ str <> ": " <> T.replace "\n" " | " (T.pack def)
--  where
--    warning = warn "Parse error in `urbandict' JSON" >> return ""

-- | Print the channel's userlist.
userlist :: Func
userlist _ = do
    edest <- sees $ currDest
    users <- sees $ M.elems . stServUsers . currServ
    liftIO $ putStrLn "Users" >> print edest >> print users
    return $ either origNick (chanNicks users) edest
  where
    chanNicks us c = let nicks = filter (M.member (stChanName c) . userChans) us
                         nicks' = filter ((/= Offline) . userStat) nicks
                     in T.unwords $ map origNick nicks'

-- | Set the Config verbosity
verbosity :: Func
verbosity str = do
    if T.null $ T.strip str
    then T.pack . show . stConfVerb <$> readConfig
    else mwhenUserStat (== BotOwner) $ do
        mapConfig $ \c -> c { stConfVerb = let v = stConfVerb c
                                               mv = readMay $ T.unpack str
                                           in maybe v id mv
                            }
        return ""

-- | Give voice to users.
voice :: Func
voice str = mode $ "+" <> vs <> " " <> T.unwords nicks
  where
    nicks = T.words str
    vs = T.replicate (length nicks) "v"


data WikiPage = WikiPage { wikipPageid :: !Int
                         , wikipNs :: !Int
                         , wikipTitle :: !Text
                         , wikipExtract :: !Text
                         }
                         deriving (Generic, Show)
instance FromJSON WikiPage where
    parseJSON = lowerFromJSON 5

data WikiQuery = WikiQuery { wikiqNormalized :: Maybe Value
                           , wikiqRedirects :: Maybe Value
                           , wikiqPages :: Map Text WikiPage
                           }
                           deriving (Show, Generic)
instance FromJSON WikiQuery where
    parseJSON = lowerFromJSON 5

data WikiBase = WikiBase { wikiBatchcomplete :: !Text
                         , wikiQuery :: !WikiQuery
                         }
                         deriving (Show, Generic)
instance FromJSON WikiBase where
    parseJSON = lowerFromJSON 4

-- | Wikipedia summary fetching.
wiki :: Func
wiki str = do
    let (lang', str') = bisect (== ' ') str
        (lang, queryStr) = if isLang lang' && not (T.null $ T.strip str')
                        then (lang', str')
                        else ("en", str)

        url = concat ["https://"
                     , T.unpack lang
                     , ".wikipedia.org/w/api.php"
                     , "?format=json&action=query&prop=extracts"
                     , "&exintro=&explaintext=&redirects=1&titles="
                     , urlEncode $ T.unpack queryStr
                     ]

        opts = defaults & header "User-Agent" .~ [T.encodeUtf8 version]

    !er <- try $ getWith opts url

    when (isLeft er) $ liftIO $ print er

    let mextract = do
            obj <- join $ decode . (^. responseBody) <$> hush er
            (pg, _) <- M.maxView . wikiqPages $ wikiQuery obj
            return $ T.take 1000 $ wikipExtract pg

    return $ maybe "" id mextract

  where
    isLang x = T.length x == 2 || x == "simple"

