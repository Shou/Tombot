
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

import System.Random (randomRIO)

import Text.JSON
import Text.Regex
import Text.XML.Light

-- }}}


funcs :: Map Text Func
funcs = M.fromList [ ("b", ban)
                   , (">", echo)
                   , ("<", priv)
                   , ("k", kick)
                   , ("m", mode)
                   , ("del", del)
                   , ("gay", gay)
                   , ("raw", raw)
                   , ("sed", sed)
                   , ("set", set)
                   , ("v", voice)
                   , ("an", anime)
                   , ("ma", manga)
                   , ("^", history)
                   , ("ai", airing)
                   , ("dict", dict)
                   , ("eval", eval)
                   , ("help", help)
                   , ("http", http)
                   , ("isup", isup)
                   , ("kill", kill)
                   , ("len", count)
                   , ("let", store)
                   , ("ra", random)
                   , ("tell", tell)
                   , ("wiki", wiki)
                   , ("funcs", list)
                   , ("kb", kickban)
                   , ("about", about)
                   , ("event", event)
                   , ("greet", greet)
                   , ("sleep", sleep)
                   , ("title", title)
                   , ("topic", topic)
                   , ("show", reveal)
                   , ("us", userlist)
                   , ("cutify", cutify)
                   , ("reload", reload)
                   , ("urb", urbandict)
                   , ("britify", britify)
                   ]


-- TODO
about :: Func
about _ = return ""

-- XXX wait, isn't this what `store' is for?
--     or was it `let'?
-- TODO
alias :: Func
alias str = return ""

-- TODO filters
-- | Low level anime releases function, instead returning a list of strings.
anime' :: Text -> Mind [String]
anime' str = do
    let (tn, str') = T.break (== ' ') $ T.stripStart str
        mn = readMay $ T.unpack tn :: Maybe Int
        n = maybe 10 id mn
        string = maybe str (const str') mn
    -- TODO filter
    --let (str', filters) = foldr' sepFilter ("", []) $ words str
        burl = "http://www.nyaa.eu/?page=search&cats=1_37&filter=2&term="
    -- TODO urlEncode'
    html <- liftIO $ httpGetString (T.unpack $ burl <> string)
    let elem' = fromMaybeElement $ parseXMLDoc html
        qname = QName "td" Nothing Nothing
        attrs = [Attr (QName "class" Nothing Nothing) "tlistname"]
        elems = findElementsAttrs qname attrs elem'
        -- TODO strip . elemsText
        animes = map (elemsText) elems
        -- TODO filter
        --animes' = filter (\x -> not $ any (`isInfixOf` x) filters) animes
    return $ take n animes

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
        string = T.unpack $ maybe str (const str') mn
        url = "http://www.mahou.org/Showtime/?o=ET"
        isSearch = not $ null string
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
                let search = map (`isInfixOf` map toLower series)
                in if and . search . words $ map toLower string
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

-- | Replace words with British slang equivalents.
britify :: Func
britify str = do
    tmvar <- gets $ currConfigTMVar
    bs <- liftIO $ do
        dir <- atomically $ confDir . fst <$> readTMVar tmvar
        ml <- readConfig $ dir <> "britify"
        return $ maybe [] id ml
    return $ wordReplace str bs

-- XXX what purpose does this function serve
colorize :: Func
colorize str = do
    return ""

-- TODO
-- | Replace rude/lascivious words with cute ones.
cutify :: Func
cutify str = do
    tmvar <- gets $ currConfigTMVar
    bs <- liftIO $ do
        dir <- atomically $ confDir . fst <$> readTMVar tmvar
        ml <- readConfig $ dir <> "cutify"
        return $ maybe [] id ml
    return $ wordReplace str bs

-- TODO
-- | Delete something stored with `store'.
del :: Func
del str = return ""

-- TODO
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

-- TODO
-- XXX this needs to be, like eval, right associative.
-- | Create an event. Useful with `sleep'.
event :: Func
event str = whenStat (>= AdminStat) $ return ""

-- | Rainbow text!
gay :: Func
gay str = return $ colorize 0 mempty str <> "\ETX"
  where
    colorize n acc t
        | T.null t = acc
        | T.head t == ' ' = colorize n (acc <> " ") (T.tail t)
        | otherwise = let n' = succ n
                          color = colors !! mod n (length colors)
                          char' = T.singleton $ T.head t
                          acc' = acc <> color <> char'
                          t' = T.tail t
                      in colorize n' acc' t'
    colors = ["\ETX04", "\ETX07", "\ETX08", "\ETX03", "\ETX02", "\ETX06"]

-- TODO several users
-- | Kick a user.
kick :: Func
kick str = do
    edest <- gets $ currDest
    let e = flip fmap edest $ write . fullkick
    either (const $ return "") (>> return "") e
  where
    (nicks', txt) = T.break (== ':') str
    nicks = T.intercalate "," $ T.words nicks'
    fullkick c = "KICK " <> stChanName c <> " " <> nicks <> " " <> txt

-- TODO several users
-- | Kick and ban a user.
kickban :: Func
kickban str = mapM_ ($ str) [kick, ban] >> return ""

count :: Func
count str = do
    return $ T.pack . show $ T.length str

-- | List the available Funcs.
list :: Func
list _ = do
    edest <- gets $ currDest
    tmvar <- gets currConfigTMVar
    funcs <- fmap (confFuncs . fst) . liftIO $ atomically $ readTMVar tmvar
    let ea = fmap stChanFuncs edest
        ef = flip fmap ea $ allow (M.keys funcs \\) id
        fs = either (const []) id ef
    return $ T.unwords fs

-- TODO make this right associative
-- | Evaluate KawaiiLang.
eval :: Func
eval str = do
    current <- get
    let configt = currConfigTMVar current
        server = currServ current
    funcs <- fmap (confFuncs . fst) $ liftIO $ atomically $ readTMVar configt
    let edest = currDest current
        e = flip fmap edest $ \chan -> do
            let parser = botparser (stChanPrefix chan) (M.keys funcs)
                mkl = A.maybeResult . flip A.feed "" $ A.parse parser str
                me = flip fmap mkl $ compile funcs
            maybe (return "") id me
    either (\x -> warn x >> return "") id e

-- TODO reconnect on Handle error
--      - Make a function that does this for us.
-- TODO specify server/channel
-- FIXME this isn't a `raw' replacement!!!
-- | Global message.
glob :: Func
glob str = whenStat (>= AdminStat) $ do
    tmvar <- gets $ currConfigTMVar
    mhs <- fmap snd $ liftIO $ atomically $ readTMVar tmvar
    void . forkMi $ forM_ (M.elems mhs) $ \h -> do
        e <- liftIO $ do
            n <- randomRIO (3, 6)
            e <- E.try $ T.hPutStrLn h $ T.take 420 str
            threadDelay (10^6 * n)
            return e
        either onerror return e
    return ""
  where
    onerror :: SomeException -> Mind ()
    onerror e = erro e

-- TODO add more greetings
-- | Greeting from the bot.
greet :: Func
greet str = do
    tmvar <- gets currConfigTMVar
    dir <- liftIO . fmap (confDir . fst) . atomically $ readTMVar tmvar
    mgreets <- readConfig $ dir <> "greet"
    let len = maybe 0 length mgreets
    n <- liftIO $ randomRIO (0, len - 1)
    return $ maybe "" (flip (atDef "") n) mgreets

-- XXX should be left associative like `eval'
-- TODO add help for the operators, help syntax and other relevant things
-- | Help for usage of the bot.
help :: Func
help str = do
    let str' = T.strip $ T.toLower str
    tmvar <- gets currConfigTMVar
    dir <- liftIO . fmap (confDir . fst) . atomically $ readTMVar tmvar
    helps <- readConfig $ dir <> "help"
    let mhelp = join $ M.lookup str <$> helps
    return $ maybe "" id mhelp

-- TODO filter
-- FIXME empty strings return nothing
-- | Search the bot's logs and return a matching message if any.
history :: Func
history str = do
    tmvar <- gets currConfigTMVar
    host <- stServHost <$> gets currServ
    edest <- gets currDest
    path <- fmap (confLogPath . fst) $ liftIO $ atomically $ readTMVar tmvar
    let dest = T.unpack $ either userNick stChanName edest
        path' = path <> host <> " " <> dest
        (tn, str') = T.break (== ' ') $ T.stripStart str
        mn = readMay $ T.unpack tn :: Maybe Int
        n = maybe 1 id mn
        string = maybe str (const str') mn
    ts <- reverse . T.lines <$> liftIO (T.readFile path')
    let ts' = flip filter ts $ flip any (T.words str') . flip T.isInfixOf
        mt = ts' `atMay` n
    return $ maybe "" id mt
  where
    fil x y | T.null x = True
            | T.null y = False
            | T.head x == '-' = T.tail x /= y
            | otherwise = x == y

-- | Get a HTTP header from a request.
http :: Func
http str = do
    (_, hed, _, _) <- httpGetResponse (T.unpack str)
    return $ maybe "" T.pack $ lookup (T.unpack httpType) hed
  where
    (httpType, httpURL) = bisect (== ' ') str

-- TODO
-- | Check if a website is up.
isup :: Func
isup str = do
    let url = "http://" <> foldr (flip T.replace "") str ["https://", "http://"]
    (_, _, status, _) <- httpGetResponse (T.unpack url)
    return $ T.pack status

-- TODO
-- | Kill an event
kill :: Func
kill str = return ""

-- TODO filter
-- | Recent manga releases.
manga :: Func
manga str = do
    let (tn, str') = T.break (== ' ') $ T.stripStart str
        mn = readMay $ T.unpack tn :: Maybe Int
        n = maybe 10 id mn
        string = maybe str (const str') mn
        burl = "https://www.mangaupdates.com/releases.html?act=archive&search="
    -- TODO urlEncode
    html <- httpGetString (T.unpack $ burl <> string)
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

-- | Set the channel mode.
mode :: Func
mode str = whenStat (>= OpStat) $ do
    edest <- gets $ currDest
    let e = flip fmap edest $ write . fullmode
    either (const $ return "") (>> return "") e
  where
    (m, s) = T.break (== ' ') $ T.strip str
    fullmode c = "MODE " <> m <> " " <> stChanName c <> " " <> s

-- | Send a private message to the user.
priv :: Func
priv str = do
    nick <- gets $ userNick . currUser
    write $ "PRIVMSG " <> nick <> " :" <> str
    return ""

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
raw str = whenStat (>= AdminStat) $ write str >> return ""

-- TODO
-- FIXME this explodes
-- | Reload the bot's Config and Funcs.
reload :: Func
reload _ = whenStat (>= AdminStat) $ do
    confpath <- confPath . fst <$> readConfHands
    ec <- loadModules [confpath] ["Config"] "config" (H.as :: Config)
    let e = flip fmap ec $ \config -> do
        verb $ confVerbosity config
        verb $ confLogging config
        verb $ confPath config
        verb $ confModules config
        verb $ map fst $ M.toList $ confFuncs config
        mapConfHands $ first (const config)
        return ""
    either (\x -> warn x >> return "") id e
  where
    loadModules ps ms x as = liftIO $ H.runInterpreter $ do
        H.loadModules ps
        H.setTopLevelModules ms
        H.interpret x as

-- TODO
-- | Remind a user on login.
remind :: Func
remind str = do
    return ""

-- TODO
-- XXX make this right associative
-- | Respond to a regex match by running a function with any match(es) as the
--   argument.
--
-- > :on /http:\/\/\S+/ title \0
-- > :on /what should i do/i ra Do nothing at all!|S-schlick!|Do your work!|Stop procrastinating!
respond :: Func
respond str = whenStat (>= OpStat) $ do
    return ""

-- | Show StateT data.
reveal :: Func
reveal str = do
    case double of
        (_, "") -> return "show <key> <argument>"
        ("User", nick) -> getUser nick >>= return . maybe "" (T.pack . show)
        ("Chan", chan) -> getChan chan >>= return . maybe "" (T.pack . show)
        _ -> return $ "Not a key. Keys: " <> T.intercalate ", " keys
  where
    key = T.strip . fst . bisect (== ' ') . fst . bisect (== '=')
    arg = T.strip . snd . bisect (== ' ') . fst . bisect (== '=')
    double = (key str, arg str)
    keys = [ "User"
           , "Chan"
           ]

-- TODO Char x should only be a specific range of chars. No alphanumeric or '\'
-- | Regex replace function.
--
-- > .sed s\/apples\/Google\/i I love apples!
sed :: Func
sed str = do
    mwhen (T.length str > 1) $ do
        let c = str `T.index` 1
            m = A.maybeResult . flip A.feed "" $ A.parse (parser c) str
        flip (maybe $ pure "") m $ \x@(mat, rep, ins, str') -> do
            liftIO $ verb x
            let regex = mkRegexWithOpts mat False ins
            e <- liftIO $ E.try (pure $! subRegex regex str' rep)
            case (e :: Either E.SomeException String) of
                Right a -> pure $ T.pack a
                Left e -> pure ""
  where
    parser :: Char -> Parser (String, String, Bool, String)
    parser x = do
        A.char 's'
        A.char x
        (mat, mc) <- manyTillKeep A.anyChar $ escape x
        (rep, rc) <- manyTillKeep A.anyChar $ escape x
        ins <- (== 'i') <$> A.try (A.char 'i' <|> pure 'z')
        A.space
        str <- T.unpack <$> A.takeText
        pure (mat <> [mc], rep <> [rc], ins, str)
    escape x = A.try $ A.notChar '\\' >>= \c -> A.char x >> return c

-- TODO
-- | `set' lets the user change settings, such as the `UserStat' of a user.
set :: Func
set str = whenStat (>= OpStat) $ do
    server <- gets currServ
    tmvar <- gets currConfigTMVar
    config <- liftIO $ atomically $ readTMVar tmvar
    liftIO $ print triple
    case triple of
        (_, _, "") -> return "set <key> <argument> = <value>"
        ("ConfVerbosity", _, value) -> do
            let mv = readMay $ T.unpack value :: Maybe Int
            return . T.pack $ show mv
        ("keys", _, _) -> return $ "Keys: " <> T.intercalate ", " keys
        (_, "", _) -> return "set <key> <argument> = <value>"
        ("UserStat", arg, value) -> do
            let mv = readMay $ T.unpack (value <> "Stat") :: Maybe UserStatus
            whenStat (> maybe UserStat id mv) $ do
                e <- modUser arg $ \u ->
                    let stat = userStat u
                    in u { userStat = maybe stat id mv }
                return $ either id (const "") e
        ("ChanJoin", arg, value) -> do
            let mv = readMay $ T.unpack value :: Maybe Bool
            e <- modChanJoin arg $ \b -> maybe b id mv
            return $ either id (const "") e
        ("ChanAutoJoin", arg, value) -> do
            let mv = readMay $ T.unpack value :: Maybe Bool
            e <- modChanAutoJoin arg $ \b -> maybe b id mv
            return $ either id (const "") e
        ("ChanPrefix", arg, value) -> do
            e <- modChanPrefix arg $ const $ T.unpack value
            return $ either id (const "") e
        ("ChanFuncs", arg, value) -> do
            let mv = readMay $ T.unpack value :: Maybe (Allowed [Text])
                insert = case mv of
                    Just (Whitelist xs)
                        | "set" `elem` xs -> const $ Whitelist xs
                        | otherwise -> const $ Whitelist $ "set" : xs
                    Just (Blacklist xs)
                        | "set" `notElem` xs -> const $ Blacklist xs
                        | otherwise -> const $ Blacklist $ filter (/= "set") xs
                    Nothing -> id
            e <- modChanFuncs arg $ insert
            return $ either id (const "") e
        ("ServBotNick", arg, value) -> do
            -- TODO only allow valid nicks
            -- let mh = stServHost server `M.lookup` snd config
            return . T.pack $ show value
        ("Channel", arg, value) -> return ""
        _ -> return $ "Not a key. Keys: " <> T.intercalate ", " keys
  where
    key = T.strip . fst . bisect (== ' ') . fst . bisect (== '=')
    arg = T.strip . snd . bisect (== ' ') . fst . bisect (== '=')
    value = T.strip . snd . bisect (== '=')
    triple = (key str, arg str, value str)
    keys = [ "UserStat"
           , "ChanJoin"
           , "ChanAutoJoin"
           , "ChanPrefix"
           , "ChanFuncs"
           , "ServBotNick"
           , "Channel"
           , "ConfVerbosity"
           ]

-- | Delay a function by n seconds.
sleep :: Func
sleep str = do
    let mn = readMay (T.unpack $ T.strip str) :: Maybe Double
    case mn of
        Just n -> liftIO $ threadDelay $ fromEnum $ 10^6*n
        _ -> return ()
    return ""

-- TODO
-- XXX Admin or above required
--      - Okay, maybe Op? Or just User...
--     Isn't this what `let' is supposed to do?
-- | `store' adds a new func to the Map and runs the contents through `eval'.
store :: Func
store str = return ""

-- | Store a message for a user that is printed when they talk next.
tell :: Func
tell str = do
    serv <- gets $ stServHost . currServ
    edest <- gets $ currDest
    cnick <- gets $ userNick . currUser
    tmvar <- gets currConfigTMVar
    dir <- liftIO . fmap (confDir . fst) . atomically $ readTMVar tmvar
    mtells <- readConfig $ dir <> "tell"
    let dest = either userName stChanName edest
        mchans = join $ M.lookup serv <$> mtells
        musers = join $ M.lookup dest <$> mchans
        mtexts = join $ M.lookup nick <$> musers
        msgs = maybe [] id mtexts ++ [msg cnick]
        users = maybe (M.singleton nick msgs) (M.insert nick msgs) musers
        chans = maybe (M.singleton dest users) (M.insert dest users) mchans
        servers = maybe (M.singleton serv chans) (M.insert serv chans) mtells
    liftIO $ do
        print edest
        print cnick
    e <- writeConfig (dir <> "tell") servers
    return $ either id (const "") e
  where
    (nick, msg') = bisect (== ' ') str
    msg c = T.take 400 msg' <> " (from " <> c <> ")"

-- | Website title fetching function.
title :: Func
title str = do
    (con, hed, _, _) <- httpGetResponse (T.unpack str)
    let respType = maybe "" id $ lookup "Content-Type" hed
    liftIO $ print respType
    mwhen ("text/html" `isInfixOf` respType) $ do
        let xml = fromMaybeElement $ parseXMLDoc con
            qTitle = QName "title" Nothing Nothing
            elem = fromMaybeElement $ findElement qTitle xml
            text = elemsText elem
        return $ T.pack text

-- TODO
-- | Channel topic changing function.
topic :: Func
topic str = whenStat (>= OpStat) $ do
    edest <- gets $ currDest
    let e = flip fmap edest $ \c -> do
        let t = modder $ stChanTopic c
        unless (t == stChanTopic c) $ do
            write $ "TOPIC " <> stChanName c <> " :" <> t
            write $ "TOPIC " <> stChanName c
        return t
    either (const $ return "") id e
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
userlist _ = whenStat (>= OpStat) $ do
    edest <- gets $ currDest
    users <- gets $ M.elems . stServUsers . currServ
    return $ either userNick (chanNicks users) edest
  where
    chanNicks us c = let nicks = filter (M.member (stChanName c) . userChans) us
                         nicks' = filter ((/= OfflineStat) . userStat) nicks
                     in T.unwords $ map userNick nicks'

-- | Give voice to users.
voice :: Func
voice str = mode $ "+" <> vs <> " " <> T.unwords nicks
  where
    nicks = T.words str
    vs = T.replicate (length nicks) "v"

-- TODO
-- | Wikipedia summary fetching.
wiki :: Func
wiki str = do
    let burl = "https://en.wikipedia.org/w/index.php?search="
        url = T.unpack $ burl <> T.replace "+" "%20" str
    html <- liftIO $ httpGetString url
    let xml = fromMaybeElement $ parseXMLDoc html
        qDiv = QName "div" (Just "http://www.w3.org/1999/xhtml") Nothing
        qMwcontent = [Attr (QName "id" Nothing Nothing) "mw-content-text"]
        element = fromMaybeElement $ findElementAttrs qDiv qMwcontent xml
        qPar = QName "p" Nothing Nothing
        intro = findChild qPar element
        qMwsearch = [Attr (QName "class" Nothing Nothing) "mw-search-createlink"]
        search = findElementAttrs (QName "p" Nothing Nothing) qMwsearch element
        -- TODO add more fallbacks
        result = search <|> intro
        text = elemsText . fromMaybeElement $ result
    return . T.strip $ T.pack text

