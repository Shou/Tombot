
-- This file is part of Tombot, licensed under the GNU GPL 2 license.
-- See the file "LICENSE" for more information.
-- Copyright Shou, 2013

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Tombot.Funcs (funcs) where

-- {{{ Imports

import Tombot.Types
import Tombot.Utils
import Tombot.Parser

import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Concurrent.STM
import Control.Error
import Control.Exception (SomeException)
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.State

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Language.Haskell.Interpreter as H

import Network.HTTP (urlEncode)

import System.IO (hClose)
import System.Random (randomRIO)

import Text.JSON
import Text.Regex
import Text.XML.Light

-- }}}


funcs :: Map Text Func
funcs = M.fromList [ ("!", ddg)
                   , ("b", ban)
                   , (">", echo)
                   , ("<", priv)
                   , ("k", kick)
                   , ("m", mode)
                   , ("del", del)
                   , ("gay", gay)
                   , ("ops", ops)
                   , ("raw", raw)
                   , ("sed", sed)
                   , ("v", voice)
                   , ("an", anime)
                   , ("in", match)
                   , ("ma", manga)
                   , ("^", history)
                   , ("ai", airing)
                   , ("bots", bots)
                   , ("dict", dict)
                   , ("eval", eval)
                   , ("help", help)
                   , ("host", host)
                   , ("http", http)
                   , ("isup", isup)
                   , ("kill", kill)
                   , ("len", count)
                   , ("let", store)
                   , ("name", name)
                   , ("nick", nick)
                   , ("quit", quit)
                   , ("ra", random)
                   , ("re", remind)
                   , ("stat", stat)
                   , ("tell", tell)
                   , ("wiki", wiki)
                   , ("+", modeplus)
                   , ("funcs", list)
                   , ("kb", kickban)
                   , ("on", respond)
                   , ("-", modeminus)
                   , ("about", about)
                   , ("cjoin", cjoin)
                   , ("event", event)
                   , ("greet", greet)
                   , ("nicks", nicks)
                   , ("sleep", sleep)
                   , ("title", title)
                   , ("topic", topic)
                   , ("show", reveal)
                   , ("tr", translate)
                   , ("us", userlist)
                   , ("cajoin", cajoin)
                   , ("cutify", cutify)
                   , ("join", chanjoin)
                   , ("part", partchan)
                   , ("prefix", prefix)
                   , ("reload", reload)
                   , ("urb", urbandict)
                   , ("verb", verbosity)
                   , ("reverse", rwords)
                   , ("britify", britify)
                   , ("connect", connectIRC)
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
    let (tn, str') = T.break (== ' ') $ T.stripStart str
        mn = readMay $ T.unpack tn :: Maybe Int
        n = maybe 10 id mn
        string = maybe str (const str') mn
        url = "http://www.mahou.org/Showtime/?o=ET"
        isSearch = not $ T.null string
        (matches, filters) = wordbreaks ((== '-') . T.head) string
        search = T.unpack $ T.unwords matches
        filters' = map (tailSafe . T.unpack) filters
    content <- httpGetString url
    let elem = fromMaybeElement $ parseXMLDoc content
        qTable = QName "table" Nothing Nothing
        aSummary = [Attr (QName "summary" Nothing Nothing) "Currently Airing"]
        table = findElementsAttrs qTable aSummary elem
        table2 = join $ fmap (findElementsIn qTable) table
        qTr = QName "tr" Nothing Nothing
        trs :: [Element]
        trs = drop 1 . join $ fmap (findElements qTr) table2
        tds :: [[String]]
        tds = fmap (fmap elemsText . contentsToEls . elContent) trs
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
        mangas = take n $ foldr f [] tds
    return . T.pack $ joinUntil ((>= 400) . length) ", " mangas
  where
    istrip = unwords . words

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
bots _ = do
    gr <- greet ""
    ga <- gay "Haskell!"
    return $ gr <> " " <> ga

-- | Replace words with British slang equivalents.
britify :: Func
britify str = do
    dir <- fmap stConfDir readConfig
    ml <- readConf $ dir <> "britify"
    let bs = maybe [] id ml
    return $ wordReplace str bs

-- | Set the channel's ChanAutoJoin value
cajoin :: Func
cajoin str = do
    dest <- either userNick stChanName <$> sees currDest
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

-- | Set the channel's ChanJoin value
cjoin :: Func
cjoin str = do
    dest <- either userNick stChanName <$> sees currDest
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

-- TODO
-- | Replace rude/lascivious words with cute ones.
cutify :: Func
cutify str = do
    dir <- fmap stConfDir readConfig
    ml <- readConf $ dir <> "cutify"
    let bs = maybe [] id ml
    return $ wordReplace str bs

-- | DuckDuckGo !bang search.
ddg :: Func
ddg str = do
    let burl = ("https://api.duckduckgo.com/?format=json&q=!" <>)
    (bdy, _, _, _) <- httpGetResponse $ burl $ urlEncode $ T.unpack str
    return $ T.take 420 . T.pack . show $ bdy

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

-- TODO
-- NOTE how to do this
-- | Difference function.
diff :: Func
diff str = return ""

-- | Print the input.
echo :: Func
echo = return

-- | Evaluate KawaiiLang.
eval :: Func
eval str = do
    current <- see
    let configt = currConfigTMVar current
        server = currServ current
    funcs <- fmap stConfFuncs $ liftIO $ atomically $ readTMVar configt
    let edest = currDest current
        e = flip fmap edest $ \chan -> do
            let parser = botparser (stChanPrefix chan) (M.keys funcs)
                mkl = A.maybeResult . flip A.feed "" $ A.parse parser str
                me = flip fmap mkl $ compile funcs
            maybe (return "") id me
    either (mvoid . warn) id e

-- | Create an event. Useful with `sleep'.
event :: Func
event str = mwhenUserStat (>= Admin) $ do
    edest <- sees $ currDest
    funcs <- fmap stConfFuncs readConfig
    let e = flip fmap edest $ \chan -> do
        kill name
        tid <- forkMi $ whileAlive $ do
            let parser = botparser (stChanPrefix chan) (M.keys funcs)
                mkl = A.maybeResult . flip A.feed "" $ A.parse parser t
                mt = flip fmap mkl $ compile funcs
            maybe (void $ kill name) (>>= putPrivmsg (stChanName chan)) mt
        sets $ \c -> c {
            currServ = (currServ c) {
                stServThreads = let tds = stServThreads $ currServ c
                                in M.insert name tid tds
                                    }
                       }
    either warn id e
    return ""
  where
    (name, t) = bisect (== ' ') str
    whileAlive m = do
        evs <- sees $ stServThreads . currServ
        if M.member name evs
        then m >> whileAlive m
        else do
            let evs' = M.delete name evs
            sets $ \c -> c { currServ = (currServ c) { stServThreads = evs' } }

-- TODO strip colors
-- | Rainbow text!
gay :: Func
gay str = return $ colorize 0 mempty str' <> "\ETX"
  where
    pad [x] = ['0', x]
    pad xs = xs
    str' = foldr (flip T.replace "") str $ do
        x <- [0 .. 15]
        return . T.cons '\ETX' . T.pack . pad $ show x
    colorize n acc t
        | T.null t = acc
        | otherwise = let n' = succ n
                          color = colors !! mod n (length colors)
                          char' = T.singleton $ T.head t
                          acc' = acc <> color <> char'
                          t' = T.tail t
                      in colorize n' acc' t'
    colors = ["\ETX,04", "\ETX,07", "\ETX,08", "\ETX,03", "\ETX,02", "\ETX,06"]

-- XXX reuse the translate code, except put it in a generalised function
-- | Kanji lookup function.
kanji :: Func
kanji str = do
    return ""

-- | Kick a user.
kick :: Func
kick str = do
    edest <- sees $ currDest
    let e = flip fmap edest $ write . fullkick
    either (const $ return "") (>> return "") e
  where
    (nicks', txt) = T.break (== ':') str
    nicks = T.intercalate "," $ T.words nicks'
    fullkick c = "KICK " <> stChanName c <> " " <> nicks <> " " <> txt

-- | Kick and ban a user.
kickban :: Func
kickban str = mvoid $ mapM_ ($ str) [kick, ban]

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

-- | Connect to an IRC server.
connectIRC :: Func
connectIRC str = mwhenUserStat (>= Admin) $ do
    mtc <- M.lookup host . stConfServs <$> readConfig
    -- Check if Curr is Disconnected, Connect if so.
    -- But if Nothing then make a new Server and connect. Fork a new thread.
    case mtc of
        Just tc -> return ()
        Nothing -> return ()
    return ""
  where
    (host, port) = first T.unpack $ bisect (== ' ') str

-- | Disconnect from an IRC server.
quit :: Func
quit str = mwhenUserStat (>= Admin) $ do
    mtc <- M.lookup (T.unpack str) . stConfServs <$> readConfig
    flip (maybe $ return "") mtc $ \tc -> do
        mvoid . liftIO . flip runStateT tc $ do
            h <- sees currHandle
            tid <- sees currThreadId
            write $ "QUIT :Good bye :c"
            liftIO $ do
                killThread tid
                hClose h

-- | Count characters.
count :: Func
count str = return $ T.pack . show $ T.length str

-- | List the available Funcs.
list :: Func
list str = do
    edest <- sees currDest
    let dest = either userNick stChanName edest
    if T.null $ T.strip str
    then do
        tmvar <- sees currConfigTMVar
        funcs <- fmap (stConfFuncs) . liftIO $ atomically $ readTMVar tmvar
        let ea = fmap stChanFuncs edest
            ef = flip fmap ea $ allow (M.keys funcs \\) id
            fs = either (const []) id ef
        return $ T.unwords fs
    else mwhenPrivileged $ do
        let mv = readMay $ allowedstr :: Maybe (Allowed [Text])
            insert = case mv of
                Just (Whitelist xs)
                    | "funcs" `elem` xs -> const $ Whitelist xs
                    | otherwise -> const $ Whitelist $ "set" : xs
                Just (Blacklist xs)
                    | "funcs" `notElem` xs -> const $ Blacklist xs
                    | otherwise -> const $ Blacklist $ filter (/= "set") xs
                Nothing -> id
        e <- modChanFuncs dest $ insert
        return $ either id (const "") e
  where
    allowedstr = let (x, y) = show . T.unpack <$> bisect (== ' ') str
                 in unwords [T.unpack x, y]

-- TODO specify server/channel
-- | Global message.
glob :: Func
glob str = mwhenUserStat (>= Admin) $ do
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
    helps <- readConf $ dir <> "help"
    let mhelp = join $ M.lookup string <$> helps
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
        n = maybe 1 id mn
        string = maybe str (const str') mn
        (matches, filters) = wordbreaks ((== '-') . T.head) string
        filters' = map (T.pack . tailSafe . T.unpack) filters
    host <- stServHost <$> sees currServ
    edest <- sees currDest
    path <- fmap stConfLogPath readConfig
    let dest = T.unpack $ either userNick stChanName edest
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
    return . onlyMsg $ maybe "" id mt
  where
    onlyMsg = snd . bisect (== '\t') . snd . bisect (== '\t')

-- | The current user's hostname
host :: Func
host _ = sees $ userHost . currUser

-- | Get a HTTP header from a request.
http :: Func
http str = do
    (_, hed, _, _) <- httpGetResponse (T.unpack httpURL)
    return $ maybe "" T.pack $ lookup (T.unpack httpType) hed
  where
    (httpType, httpURL) = bisect (== ' ') str

-- FIXME is this sufficient?
-- | Check if a website is up.
isup :: Func
isup str = do
    let url = "http://" <> foldr (flip T.replace "") str ["https://", "http://"]
    (_, _, status, _) <- httpGetResponse (T.unpack url)
    return $ if isPrefixOf "2" status then "True" else "False"

-- TODO add default Channel data
-- | Join a channel.
chanjoin :: Func
chanjoin str = do
    write $ "JOIN " <> str
    return ""

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

-- | Set the channel mode.
mode :: Func
mode str = mwhenPrivileged $ do
    edest <- sees $ currDest
    let e = flip fmap edest $ write . fullmode
    either (const $ return "") (>> return "") e
  where
    (m, s) = T.break (== ' ') $ T.strip str
    fullmode c = "MODE " <> stChanName c <> " " <> m <> " " <> s

-- | Set the channel mode, prepending a +.
modeplus :: Func
modeplus str = mode $ "+" <> str

-- | Set the channel mode, prepending a -.
modeminus :: Func
modeminus str = mode $ "-" <> str

-- | The current user's name
name :: Func
name _ = sees $ userName . currUser

-- | The current user's nick
nick :: Func
nick _ = sees $ userNick . currUser

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
    | T.null $ T.strip str = mwhenPrivileged $ do
        edest <- sees currDest
        either (const $ pure "") (parter . stChanName) edest
    | otherwise = mwhenUserStat (>= Admin) $ parter str
  where
    parter chan = mvoid $ write $ "PART " <> chan

-- | Set the channel's prefix `Char's
prefix :: Func
prefix str = do
    dest <- either userNick stChanName <$> sees currDest
    if T.null $ T.strip str
    then do
        dest <- either userNick stChanName <$> sees currDest
        mchan <- M.lookup dest . stServChans . currServ <$> see
        return $ maybe "" (T.pack . stChanPrefix) mchan
    else mwhenPrivileged $ do
        mvoid . modChan dest $ \c -> c { stChanPrefix = T.unpack str }

-- | Send a private message to the user.
priv :: Func
priv str = do
    nick <- sees $ userNick . currUser
    mvoid $ putPrivmsg nick str

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
        if len > 0
        then return $ choices !! n
        else return mempty
  where
    isDigits = T.all (`elem` ['0' .. '9'])
    maybeRead = fmap fst . listToMaybe . reads

-- | Write directly to the IRC handle of the current server.
raw :: Func
raw str = mwhenUserStat (== Root) $ mvoid $ write str

-- TODO
-- FIXME this explodes
-- | Reload the bot's Config and Funcs.
reload :: Func
reload _ = mwhenUserStat (>= Root) $ do
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
    cnick <- sees $ userNick . currUser
    let f = if T.null $ T.strip msg
            then const Nothing
            else if nick == cnick
                 then const $ Just msg
                 else id
    mvoid . modLocalStored "remind" $ M.alter f nick
  where
    (nick, msg) = bisect (== ' ') str

-- | Respond to a regex match by running a function with any match(es) as the
--   argument.
--
-- > :on /http:\/\/\S+/ title \0
-- > :on /what should i do/i ra |Nothing!|Do your work!|Stop procrastinating!
respond :: Func
respond str = mwhenPrivileged $ mwhen (T.length str > 0) $ do
    dir <- fmap stConfDir readConfig
    let c = str `T.index` 0
        m = A.maybeResult . flip A.feed "" $ A.parse parsematch str
    flip (maybe $ pure "") m $ \(mat, ins, string) -> do
        let f = if null $ dropWhile (== ' ') string
                then Nothing
                else Just (ins, string)
        mvoid . modLocalStored "respond" $ M.alter (const f) mat

-- | Show StateT data.
reveal :: Func
reveal str = do
    case double of
        ("Config", _) -> do
            t <- sees currConfigTMVar
            v <- liftIO . atomically $ readTMVar t
            return . T.pack . show $ v
        ("User", nick) -> getUser nick >>= return . maybe "" (T.pack . show)
        ("Chan", chan) -> getChan chan >>= return . maybe "" (T.pack . show)
        _ -> return ""
  where
    double = first T.strip . second T.strip $ bisect (== ' ') str

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
        mwhenUserStat (> maybe Online id mv) $ do
            e <- modUser nick $ \u ->
                let stat = userStat u
                in u { userStat = maybe stat id mv }
            return $ either id (const "") e
  where
    (nick, value) = first T.strip . second T.strip $ bisect (== '=') str

-- | Delay a function by n seconds where n is a floating point number.
sleep :: Func
sleep str = do
    let mn = readMay (T.unpack $ T.strip str) :: Maybe Double
    mvoid $ case mn of
        Just n -> liftIO $ threadDelay $ fromEnum $ 10^6*n
        _ -> return ()

-- TODO only Admins can overwrite old funcs, and they MUST exist in the
--      "letfuncs" file.
-- | `store' adds a new func to the Map and runs the contents through `eval'.
store :: Func
store str = mwhenPrivileged $ do
    dir <- fmap stConfDir readConfig
    funcs <- stConfFuncs <$> readConfig
    let f = eval . (func <>)
    mapConfig $ \c ->
        let funcs = stConfFuncs c
            funcs' = if M.member name funcs
                     then funcs
                     else M.insert name f funcs
        in c { stConfFuncs = funcs' }
    mvoid . modLocalStored "letfuncs" $ \letfuncs ->
        if M.member name funcs
        then letfuncs
        else M.insert name func letfuncs
  where
    (name, func) = first T.toLower $ bisect (== ' ') str

-- | Store a message for a user that is printed when they talk next.
tell :: Func
tell str = do
    serv <- sees $ stServHost . currServ
    edest <- sees $ currDest
    cnick <- sees $ userNick . currUser
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
    (con, hed, _, _) <- httpGetResponse (T.unpack str)
    let respType = maybe "" id $ lookup "Content-Type" hed
    mwhen ("text/html" `isInfixOf` respType) $ do
        let xml = fromMaybeElement $ parseXMLDoc con
            qTitle = QName "title" Nothing Nothing
            elem = fromMaybeElement $ findElement qTitle xml
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

-- TODO
-- | Google translate.
translate :: Func
translate str = do
    let (tl', str') = bisect (== ' ') str
        (tl, string) = if T.length tl' == 2 && T.all isLower tl'
             then (T.unpack tl', T.unpack str')
             else ("en", T.unpack str)
        url sl tl x = concat [ "http://translate.google.com"
                             , "/translate_a/t"
                             , "?client=t&hl=en"
                             , "&sl=" <> sl <> "&tl=" <> tl <> "&q=" <> x
                             ]
    jsonStr <- httpGetString $ url "auto" tl $ urlEncode string
    let m = A.maybeResult . flip A.feed "" $ A.parse parser $ T.pack jsonStr
    return . T.concat $ maybe [] id m
  where
    parser :: Parser [Text]
    parser = do
        A.char '['
        lists
    lists = do
        A.char '['
        A.many1 $ do
            tr <- list
            A.try $ A.char ',' <|> A.char ']'
            return tr
    list = do
        A.char '['
        tr <- pstring
        A.char ','
        pstring
        A.char ','
        pstring
        A.char ','
        pstring
        A.char ']'
        return tr
    pstring = do
        let a = A.string "\"\""
        let b = do
            t <- T.pack <$> inside '"'
            A.char '"'
            return t
        a <|> b

-- | Urban dictionary lookup function.
urbandict :: Func
urbandict str = do
    let burl = "http://api.urbandictionary.com/v0/define?term="
    jsonStr <- httpGetString (burl ++ T.unpack str)
    let result = decode jsonStr :: Result (JSObject JSValue)
        text = (\(Ok x) -> x) $ fromJSString
                              . (\(JSString x) -> x)
                              . fromJust
                              . lookup "definition"
                              . fromJSObject
                              . (\(JSObject x) -> x )
                              . (!! 0) . (\(JSArray xs) -> xs)
                              . fromJust
                              . lookup "list"
                              . fromJSObject
                              <$> result
    return $ str <> ": " <> T.replace "\n" " " (T.pack text)

-- | Print the channel's userlist.
userlist :: Func
userlist _ = mwhenPrivileged $ do
    edest <- sees $ currDest
    users <- sees $ M.elems . stServUsers . currServ
    return $ either userNick (chanNicks users) edest
  where
    chanNicks us c = let nicks = filter (M.member (stChanName c) . userChans) us
                         nicks' = filter ((/= Offline) . userStat) nicks
                     in T.unwords $ map userNick nicks'

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
    let burl = "https://en.wikipedia.org/w/index.php?search="
        url = T.unpack $ burl <> T.replace "+" "%20" str
    html <- liftIO $ httpGetString url
    let xml = fromMaybeElement $ parseXMLDoc html
        qA = QName "a" Nothing Nothing
        qDiv = QName "div" (Just "http://www.w3.org/1999/xhtml") Nothing
        qMwcontent = [attr "id" "mw-content-text"]
        element = fromMaybeElement $ findElementAttrs qDiv qMwcontent xml
        qPar = QName "p" Nothing Nothing
        ulPar = QName "ul" Nothing Nothing
        intro = findChild qPar element
        alts = findChild ulPar element
        qMwsearch = [attr "class" "mw-search-result-heading"]
        search = findElementAttrs (QName "p" Nothing Nothing) qMwsearch element
        sresults = fromMaybeElement $ search
        stext = elemsText . fromMaybeElement $ findElement qA sresults
        text' = elemsText . fromMaybeElement $ intro
        text = if isSuffixOf "may refer to:" text'
               then elemsText . fromMaybeElement $ alts
               else text'
    verb alts
    verb search >> verb sresults
    if isJust search
    then wiki $ T.pack stext
    else return . T.strip $ T.pack text
  where
    attr t v = Attr (QName t Nothing Nothing) v

