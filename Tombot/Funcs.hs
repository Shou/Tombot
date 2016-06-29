
-- This file is part of Tombot, licensed under the GNU GPL 2 license.
-- See the file "LICENSE" for more information.
-- Copyright Shou, 2013

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Tombot.Funcs (funcs) where

-- {{{ Imports

import Tombot.Types
import Tombot.Utils
import Tombot.Parser

import Control.Applicative
import Control.Arrow hiding (left, right)
import Control.BoolLike (Orlike((<|<)))
import Control.Concurrent
import Control.Concurrent.STM
import Control.Error
import Control.Exception (SomeException)
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Either (left, right)

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import qualified Data.CaseInsensitive as CI
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Debug.Trace

import qualified Language.Haskell.Interpreter as H

import Network.HTTP (urlDecode, urlEncode)

import System.Exit (ExitCode(ExitSuccess))
import System.IO (hClose)
import System.Random (randomRIO)
import System.Process (readProcessWithExitCode, spawnCommand)

import Text.JSON
import Text.Regex
import Text.XML.Light

-- }}}


funcs :: Map Text Funk
funcs = toFunks [ ("!", ddg, Online)
                , ("b", ban, OP)
                , (">", echo, Online)
                , ("<", priv, Online)
                , ("k", kick, OP)
                , ("m", mode, OP)
                , ("v", voice, OP)
                , ("x", exchange, Online)
                , ("^", findMsg, Online)
                , ("me", me, Online)
                , ("np", lastfm, Online)
                , ("an", anime, Online)
                , ("in", match, Online)
                , ("ma", manga, Online)
                , ("ai", airing, Online)
                , ("hs", mueval, Online)
                , ("ra", random, Online)
                , ("re", remind, Online)
                , ("kb", kickban, OP)
                , ("on", respond, Online)
                , ("us", userlist, OP)
                , ("tr", translate, Online)
                , ("ops", ops, Online)
                , ("onf", frespond, Online)
                , ("ons", responds, Online)
                , ("raw", raw, Root)
                , ("sed", sed, Online)
                , ("len", count, Online)
                , ("let", store, Online)
                , ("urb", urbandict, Online)
                , ("bots", bots, Online)
                , ("eval", eval, Online)
                , ("help", help, Online)
                , ("hist", history, Online)
                , ("host", host, Online)
                , ("http", http, Online)
                , ("isup", isup, Online)
                , ("kill", kill, Admin)
                , ("name", name, Online)
                , ("nick", nick, Online)
                , ("quit", quit, Admin)
                , ("stat", stat, Online)
                , ("tell", tell, Online)
                , ("wiki", wiki, Online)
                , ("show", reveal, Online)
                , ("join", chanjoin, Online)
                , ("part", partchan, OP)
                , ("verb", verbosity, Admin)
                , ("cjoin", cjoin, Online)
                , ("event", event, Admin)
                , ("fstat", fstat, OP)
                , ("funcs", list, Online)
                , ("kanji", kanji, Online)
                , ("nicks", nicks, Admin)
                , ("sleep", sleep, Online)
                , ("title", title, Online)
                , ("topic", topic, Online)
                , ("cajoin", cajoin, Online)
                , ("prefix", prefix, Online)
                , ("romaji", romaji, Online)
                , ("reverse", rwords, Online)
                , ("restart", restart, Root)
                , ("connect", connectIRC, Admin)
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
anime :: Text -> Mind Text
anime str = do
    animes <- anime' str
    let animes' = map (colourise . T.pack) animes
    return $ T.intercalate ", " animes'

-- TODO filter
airing :: Func
airing str = do
    let (tn', str') = bisect (== ' ') str
        mn = readMay $ T.unpack tn' :: Maybe Int
        n = maybe 10 id mn
        (o, s) = if tn' `elem` ["se", "SE", "st", "ST", "ai", "AI", "et", "ET"]
            then (tn', str')
            else ("ET", str)
        url = "http://www.mahou.org/Showtime/?o=" <> T.unpack o
        isSearch = not $ T.null s
        (matches, filters) = wordbreaks ((== '-') . T.head) s
        search = T.unpack $ T.unwords matches
        filters' = map (tailSafe . T.unpack) filters
    content <- httpGetString url
    let elem = fromMaybeElement $ parseXMLDoc content
        current = getAnimes elem "Currently Airing"
        soon = getAnimes elem "Starting Soon"
        tds :: [[String]]
        tds = current ++ soon
        f (_ : series : season : station : company : time : eta : xs) acc =
            let seri = "\ETX10" ++ series ++ "\ETX"
                tim' = "\ETX06" ++ time ++ "\ETX"
                eta' = "(" ++ istrip eta ++ ")"
                nonSearch = unwords [seri, tim', eta']
                proSearch = unwords [ "\ETX10Series\ETX:", series
                                    , "\ETX10Season\ETX:", season
                                    , "\ETX10Time\ETX:", time
                                    , "\ETX10ETA\ETX:", istrip eta
                                    , "\ETX10Station\ETX:", station
                                    , "\ETX10Company\ETX:", company
                                    ]
            in if isSearch then
                let searcher = map (`isInfixOf` map toLower series)
                in if or $ searcher filters' then acc
                   else if and . searcher . words $ map toLower search
                        then proSearch : acc
                        else acc
               else nonSearch : acc
        f _ acc = acc
        mangas = take n $ foldr f [] tds
    return . T.pack $ joinUntil ((>= 400) . length) ", " mangas
  where
    istrip = unwords . words
    getAnimes :: Element -> String -> [[String]]
    getAnimes html tname =
        let qTable = QName "table" Nothing Nothing
            aSummary = [Attr (QName "summary" Nothing Nothing) tname]
            table = findElementsAttrs qTable aSummary html
            table2 = join $ fmap (findElementsIn qTable) table
            qTr = QName "tr" Nothing Nothing
            trs = drop 1 . join $ fmap (findElements qTr) table2
        in fmap (fmap elemsText . contentsToEls . elContent) trs

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
    headers <- httpHead $ burl $ urlEncode $ T.unpack str
    let json = maybe "" id $ M.lookup "X-Response-Body-Start" headers
        mres :: Maybe (JSObject JSValue)
        mres = resultMay $ decode json
        mx = do
            o <- mres
            s <- lookup "Redirect" $ fromJSObject o
            fromJSString <$> stringMay s
    return $ maybe "" T.pack mx

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
    fs <- allfuncs
    botparse fs str >>= compile fs

-- TODO time argument
-- | Create an event. Useful with `sleep'.
event :: Func
event str = do
    d <- either origNick stChanName . currDest <$> see
    kill name
    tid <- forkMi $ whileAlive $ do
        funcs <- stConfFuncs <$> readConfig
        kl <- botparse funcs kl
        when (kl == mempty) $ void $ kill name
        t <- compile funcs kl
        putPrivmsg d t
    sets $ \c ->
        c { currServ =
            (currServ c) { stServThreads =
                let tds = stServThreads $ currServ c
                in M.insert name tid tds
                         }
          }
    return ""
  where
    (name, str') = bisect (== ' ') str
    (tim, kl) = bisect (== ' ') str'
    time = readDef 300 (T.unpack tim) :: Int
    whileAlive m = do
        evs <- sees $ stServThreads . currServ
        if M.member name evs
        then m >> whileAlive m
        else do
            let evs' = M.delete name evs
            sets $ \c -> c { currServ = (currServ c) { stServThreads = evs' } }

-- | Currency exchange function.
exchange :: Func
exchange str = do
    let url = [ "http://rate-exchange.appspot.com/currency?from=" <> T.unpack a
              , "&to=" <> T.unpack b
              ]
    jsonStr <- httpGetString $ concat url
    let mres :: Maybe (JSObject JSValue)
        mres = resultMay $ decode jsonStr
        mdubz :: Maybe Float
        mdubz = do
            o <- mres
            join . fmap floatMay . lookup "rate" $ fromJSObject o
    return $ maybe "" (T.pack . show . (x *)) mdubz
  where
    (x, str') = first (read . T.unpack) $ bisect (== ' ') str
    (a, b) = bisect (== ' ') str'

-- | Alias for `history' and only returning the message.
findMsg :: Func
findMsg str = onlyMsg <$> history str
  where
    onlyMsg = snd . bisect (== '\t') . snd . bisect (== '\t')

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
kill str = mwhenPrivileged $ do
    evs <- sees $ stServThreads . currServ
    let mev = M.lookup str evs
    flip (maybe $ return "") mev $ \ev -> do
        liftIO $ killThread ev
        let evs' = M.delete str evs
        sets $ \c -> c { currServ = (currServ c) { stServThreads = evs' } }
        return $ "Killed event `" <> str <> "'"

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
    let mres :: Maybe (JSObject JSValue)
        mres = resultMay $ decode jsonStr
        -- TODO split this up, if Nothing on any it returns Nothing
        morcs = do
            o <- mres
            jorcs <- lookup "recenttracks" $ fromJSObject o
            fromJSObject <$> objectMay jorcs
        user = do
            orcs <- morcs
            jours <- lookup "@attr" orcs
            ours <- fromJSObject <$> objectMay jours
            jsuser <- lookup "user" ours
            fromJSString <$> stringMay jsuser
        mtr = do
            orcs <- morcs
            trs <- join . fmap arrayMay $ lookup "track" orcs
            fmap fromJSObject . join . fmap objectMay $ trs `atMay` 0
        bnp = maybe False id $ do
            tr <- mtr
            jars <- lookup "@attr" tr
            ars <- fromJSObject <$> objectMay jars
            sb <- join . fmap stringMay $ lookup "nowplaying" ars
            return $ fromJSString sb == "true"
        np = if bnp then " ▸" else ""
        artist = maybe "" id $ do
            tr <- mtr
            jart <- lookup "artist" tr
            art <- fromJSObject <$> objectMay jart
            jartist <- lookup "name" art
            fromJSString <$> stringMay jartist
        blove = maybe False id $ do
            tr <- mtr
            jlove <- lookup "loved" tr
            (== "1") . fromJSString <$> stringMay jlove
        love = if blove then "\ETX13♥\ETX" else "\ETX10♫\ETX"
        title = maybe "" id $ do
            tr <- mtr
            jtitle <- lookup "name" tr
            fromJSString <$> stringMay jtitle

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
    mfuncs <- readLocalStored "letfuncs"
    liftIO $ print mhelps >> print mfuncs
    let mboth = mhelps <> mfuncs
    let mhelp = join $ M.lookup string <$> mboth
    return $ maybe "" id mhelp
  where
    string = if T.null $ T.strip str
             then "help"
             else T.strip $ T.toLower str

-- | Search the bot's logs and return a matching message if any.
history :: Func
history str = do
    let (tn, str') = T.break (== ' ') $ T.stripStart str
        mn = readMay $ T.unpack tn :: Maybe Int
        n = maybe 0 id mn
        string = maybe str (const str') mn
        (matches, filters) = wordbreaks ((== '-') . T.head) string
        filters' = map (T.pack . tailSafe . T.unpack) filters
    host <- stServHost <$> sees currServ
    edest <- sees currDest
    path <- fmap stConfLogPath readConfig
    let dest = T.unpack $ either origNick stChanName edest
        path' = path <> host <> " " <> dest
    ts <- reverse . T.lines <$> liftIO (T.readFile path')
    let ts' = do
            t <- ts
            guard $ if null matches then True
                    else all (`T.isInfixOf` t) matches
            guard $ if null filters' then True
                    else not $ any (`T.isInfixOf` t) filters'
            return t
        mt = ts' `atMay` n
    return $ maybe "" id mt

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
json str = return ""

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
ops _ = return "++ -> <- <> >< >> +>"

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

responds :: Func
responds  str = do
    mons <- readLocalStored $ "respond"
    let ons :: [(String, (Bool, Int, String))]
        ons = sortBy (comparing $ snd3 . snd) $ maybe mempty M.toList mons
    ons' <- flip filterM ons $ \(match, (ins, n, resp)) -> deci . decide $ do
        let regex = mkRegexWithOpts match False ins
        emins <- try $ return $! matchRegex regex $ T.unpack str
        when (isLeft emins) $ do
            verb ("onMatch: " <> show emins)
            left False
        let mins = either (const $ Just []) id emins
        unless (isJust mins) $ left False
        right True
    return . T.pack . intercalate ", " . map wrap . map fst $ ons'
  where
    replace a b c = T.unpack $ T.replace (T.pack a) (T.pack b) (T.pack c)
    deci :: Mind (Either Bool Bool) -> Mind Bool
    deci m = m >>= return . either id id
    wrap t = "/" <> t <> "/"

frespond :: Func
frespond str = do
    mons <- readLocalStored "respond"
    let mon :: Maybe (Bool, Int, Text)
        mon = join $ M.lookup str <$> mons
        on = maybe "" trd mon
    return on
  where
    key = T.init . T.tail $ str
    trd (_, _, x) = x

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
    let mres :: Maybe (JSObject JSValue)
        mres = resultMay $ decode jsonStr
        def = maybe "" id $ do
            o <- mres
            jlist <- lookup "list" $ fromJSObject o
            jarr <- join . fmap (`atMay` 0) $ arrayMay jlist
            ar <- fromJSObject <$> objectMay jarr
            jdef <- lookup "definition" ar
            fromJSString <$> stringMay jdef
    mwhen (not $ null def) $ do
        return $ str <> ": " <> T.replace "\n" " | " (T.pack def)
--  where
--    warning = warn "Parse error in `urbandict' JSON" >> return ""

-- | Print the channel's userlist.
userlist :: Func
userlist _ = mwhenPrivileged $ do
    edest <- sees $ currDest
    users <- sees $ M.elems . stServUsers . currServ
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
    else mwhenUserStat (== Root) $ do
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

-- XXX isn't this in danger of recursing forever?
-- TODO
-- | Wikipedia summary fetching.
wiki :: Func
wiki str = do
    let (lang', str') = bisect (== ' ') str
        (lang, str'') = if isLang lang' && not (T.null $ T.strip str')
                        then (lang', str')
                        else ("en", str)
        burl = "https://" <> lang <> ".wikipedia.org/w/index.php?search="
        url = T.unpack $ burl <> T.replace "+" "%20" str''
    html <- liftIO $ httpGetString url
    let xml = fromMaybeElement $ parseXMLDoc html
        qA = QName "a" Nothing Nothing
        qDiv = QName "div" (Just "http://www.w3.org/1999/xhtml") Nothing
        qMwcontent = [attr "id" "mw-content-text"]
        element = fromMaybeElement $ findElementAttrs qDiv qMwcontent xml
        qPar = QName "p" Nothing Nothing
        ulPar = QName "ul" Nothing Nothing
        intro = findChild qPar element
        alts = fromMaybeElement $ findChild ulPar element
        atext = elemsText . fromMaybeElement $ findElement qA alts
        qMwsearch = [attr "class" "mw-search-result-heading"]
        search = findElementAttrs (QName "div" Nothing Nothing) qMwsearch element
        sresults = fromMaybeElement $ search
        stext = elemsText . fromMaybeElement $ findElement qA sresults
        text = elemsText . fromMaybeElement $ intro
    case () of
      _ | isJust search -> wiki $ T.pack stext
        | isMulti text -> wiki $ T.pack atext
        | otherwise -> return . T.strip $ T.pack text
  where
    isMulti t = any (`isSuffixOf` t) [ "may refer to:" :: String
                                     , "can stand for:"
                                     ]
    isLang x = T.length x <= 3 || x == "simple"
    attr t v = Attr (QName t Nothing Nothing) v

