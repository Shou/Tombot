
-- This file is part of Tombot, licensed under the GNU GPL 2 license.
-- See the file "LICENSE" for more information.
-- Copyright Shou, 2013

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Tombot.Utils where

-- {{{ Imports
import Tombot.Types

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Error
import Control.Exception (SomeException)
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.State

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as BU
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Char
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Network.HTTP (urlEncode)
import Network.HTTP.Conduit
import Network.HTTP.Conduit.Browser
import Network.HTTP.Types.Status

import System.Directory (copyFile, removeFile)
import System.IO (hClose, openTempFile, Handle)
import System.Timeout (timeout)

import Text.JSON
import Text.XML.Light
-- }}}

version :: Text
version = "Tombot 0.2.0 (the cute :3c)"

-- {{{ Attoparsec utils

-- | manyTill but return the end result as well.
manyTillKeep p end = scan ([], [])
  where
    scan (xs, mv) = fmap (xs,) end <|> (fmap (snoc xs) p >>= scan . (,mv))
    snoc xs x = xs `mappend` [x]

-- | Parse something in between two `Char's. Does not consume the last `Char'.
--
-- Note that a backward slash will escape the `Char'.
inside :: Char -> Parser String
inside x = A.many' $ escesc <|> escape x <|> A.notChar x
  where
    escesc = A.char '\\' >> A.char '\\'
    escape x = A.char '\\' >> A.char x

-- | Parse a regex match.
parsematch :: Parser (String, Bool, String)
parsematch = do
    x <- A.satisfy (not . isAlphaNum)
    mat <- inside x
    A.char x
    ins <- fmap (/= 'i') $ A.try (A.char 'i' <|> return 'z')
    text <- T.unpack <$> (A.try A.space >> A.takeText) <|> return ""
    return (mat, ins, text)

-- | Parse a sed regex replace.
parsesed :: Parser (String, String, Bool, String)
parsesed = do
    A.char 's'
    x <- A.satisfy (not . isAlphaNum)
    mat <- inside x
    A.char x
    rep <- inside x
    A.char x
    ins <- fmap (/= 'i') . A.try $ A.char 'i' <|> return 'z'
    text <- T.unpack <$> (A.try A.space >> A.takeText) <|> return ""
    return (mat, rep, ins, text)

-- | Google translate parser.
transparser :: Parser ([[Text]], (Text, [Text]))
transparser = A.char '[' >> lists
  where
    lists :: Parser ([[Text]], (Text, [Text]))
    lists = do
        A.char '['
        trans <- A.many' $ do
            tr <- list
            A.char ',' <|> A.char ']'
            return tr
        A.char ','
        defin <- def <|> return ("", [])
        return (trans, defin)
    def = do
        A.string "[["
        t <- pstring
        A.char ','
        means <- list
        return (t, means)
    list :: Parser [Text]
    list = do
        A.char '['
        A.many' $ do
            s <- pstring
            A.try $ A.char ',' <|> A.char ']'
            return s
    pstring = do
        A.char '"'
        t <- T.pack <$> inside '"'
        A.char '"'
        return t

-- }}}

-- {{{ Config utils

-- | `either' function for `Allowed' data.
allow :: (a -> b) -> (a -> b) -> Allowed a -> b
allow f _ (Blacklist a) = f a
allow _ g (Whitelist a) = g a

defStChan = StChannel { stChanName = ""
                      , stChanTopic = ""
                      , stChanJoin = False
                      , stChanAutoJoin = False
                      , stChanMode = ""
                      , stChanPrefix = ":"
                      , stChanFuncs = Whitelist ["funcs"]
                      }

defStServ = StServer { stServHost = mempty
                     , stServPort = 6667
                     , stServChans = mempty
                     , stServBotNicks = mempty
                     , stServBotName = mempty
                     , stServNickServId = Nothing
                     , stServStat = Disconnected
                     , stServUsers = mempty
                     , stServThreads = mempty
                     }

defUser = User { userNick = ""
               , userName = ""
               , userHost = ""
               , userStat = Online
               , userChans = M.empty
               }

-- | Get the `Channel'.
getChan :: Text -> Mind (Maybe StChannel)
getChan chan = do
    server <- sees currServ
    let cs = stServChans server
    return $ M.lookup chan cs

-- | Get a StChannel record field.
getChanField :: Text -> (StChannel -> a) -> Mind (Maybe a)
getChanField chan f = do
    channel <- getChan chan
    return $ fmap f channel

getUser :: Nick -> Mind (Maybe User)
getUser nick = sees currServ >>= return . M.lookup nick . stServUsers

mapChans f user = user { userChans = f $ userChans user }

mapStat f user = user { userStat = f $ userStat user }

-- | Modify the server's userlist.
modUserlist f = do
    s <- sees $ currServ
    sets $ \k -> k { currServ = s { stServUsers = f $ stServUsers s } }

-- | Modify `Channel' data.
modChan :: Text -> (StChannel -> StChannel) -> Mind (Either Text ())
modChan chan f = do
    server <- sees currServ
    let cs = stServChans server
        mc = M.lookup chan cs
        ec = note ("No channel: " <> chan) mc
        cs' = maybe cs (flip (M.insert chan) cs . f) mc
    sets $ \k -> k { currServ = server { stServChans = cs' } }
    return $ void ec

-- | Change the topic of a channel.
modChanTopic :: Text -> (Text -> Text) -> Mind (Either Text ())
modChanTopic chan f = modChan chan $ \c -> c { stChanTopic = f $ stChanTopic c }

-- | Change the Funcs of a channel.
modChanFuncs :: Text -- ^ Channel
             -> (Allowed [Text] -> Allowed [Text]) -> Mind (Either Text ())
modChanFuncs chan f = modChan chan $ \c -> c { stChanFuncs = f $ stChanFuncs c }

-- | Change whether the bot is allowed to join a channel.
modChanJoin :: Text -> (Bool -> Bool) -> Mind (Either Text ())
modChanJoin chan f = modChan chan $ \c -> c { stChanJoin = f $ stChanJoin c }

-- | Change whether the bot should auto-join on reconnect/kick.
modChanAutoJoin :: Text -> (Bool -> Bool) -> Mind (Either Text ())
modChanAutoJoin chan f = modChan chan $ \c ->
    c { stChanAutoJoin = f $ stChanAutoJoin c }

-- | Change the function prefix characters.
modChanPrefix :: Text -> ([Char] -> [Char]) -> Mind (Either Text ())
modChanPrefix chan f = modChan chan $ \c -> c { stChanPrefix = f $ stChanPrefix c }

-- | Modify User data.
modUser :: Nick -> (User -> User) -> Mind (Either Text ())
modUser user f = do
    server <- sees currServ
    let us = stServUsers server
        mu = M.lookup user us
        eu = note ("No user: " <> CI.original user) mu
        us' = maybe us (flip (M.insert user) us . f) mu
    sets $ \k -> k { currServ = server { stServUsers = us' } }
    return $ void eu

-- | From `Channel' to `StChannel'
toStChan :: Channel -> StChannel
toStChan (Channel name join ajoin prefix funcs) =
    let stName = T.pack name
        stFuncs = map T.pack <$> funcs
    in StChannel { stChanName = stName
                 , stChanTopic = ""
                 , stChanJoin = join
                 , stChanAutoJoin = ajoin
                 , stChanMode = ""
                 , stChanPrefix = prefix
                 , stChanFuncs = stFuncs
                 }

-- | From `Config' to `StConfig'
toStConf :: Config -> StConfig
toStConf (Config v d lg lp p fs) = StConfig { stConfVerb = v
                                            , stConfDir = d
                                            , stConfLog = lg
                                            , stConfLogPath = lp
                                            , stConfPath = p
                                            , stConfFuncs = fs
                                            , stConfServs = M.empty
                                            }

-- | From `Server' to `StServer'
toStServ :: Server -> StServer
toStServ (Server host port chans nicks name nsid) =
    let stChans = map (\c -> (T.pack $ chanName c, toStChan c)) chans
        stNSId = if null nsid then Nothing else Just (T.pack nsid)
    in StServer { stServHost = host
                , stServPort = fromIntegral port
                , stServChans = M.fromList $ stChans
                , stServBotNicks = map T.pack nicks
                , stServBotName = T.pack name
                , stServNickServId = stNSId
                , stServStat = Connected
                , stServUsers = M.empty
                , stServThreads = mempty
                }

-- | When predicate `p' is True, run `m', otherwise return `()'.
whenStat :: (Either [Char] UserStatus -> Bool) -> Mind () -> Mind ()
whenStat p m = do
    dest <- either origNick stChanName <$> sees currDest
    chans <- sees $ userChans . currUser
    stat <- sees $ userStat . currUser
    let isMode = maybe False (p . Left) $ M.lookup dest chans
        isStat = p $ Right stat
    case () of
      _ | isMode -> m
        | isStat -> m
        | otherwise -> return ()

-- | When predicate `p' is True, run `m', otherwise return `mempty'.
mwhenStat :: Monoid a => (Either [Char] UserStatus -> Bool) -> Mind a -> Mind a
mwhenStat p m = do
    dest <- either origNick stChanName <$> sees currDest
    chans <- sees $ userChans . currUser
    stat <- sees $ userStat . currUser
    let isMode = maybe False (p . Left) $ M.lookup dest chans
        isStat = p $ Right stat
    case () of
      _ | isMode -> m
        | isStat -> m
        | otherwise -> return mempty

mwhenPrivileged :: Monoid a => Mind a -> Mind a
mwhenPrivileged = mwhenStat (either isMod (>= OP))

mwhenUserStat :: Monoid a => (UserStatus -> Bool) -> Mind a -> Mind a
mwhenUserStat p = mwhenStat (either (const False) p)

mwhenPrivTrans :: Monoid a => UserStatus -> Mind a -> Mind a
mwhenPrivTrans u = mwhenStat (either isPriv (>= u))
  where
    isPriv = if u >= OP then isMod else const False

unlessBanned :: Mind () -> Mind ()
unlessBanned m = whenStat (either (const False) (/= Banned)) m

origNick :: User -> Text
origNick = CI.original . userNick

-- | From a list of tri-tuple funcs to Funks
toFunks :: [(Text, Func, UserStatus)] -> Map Text Funk
toFunks = M.fromList . map (\(n, f, s) -> (n, Funk n f s))

-- }}}

-- {{{ Decide utils

-- | Run an EitherT monad inside Mind.
decide :: Decide e a -> Mind (Either e a)
decide m = runEitherT m

-- | Run an EitherT monad and print a warning if `left'.
warnDecide :: Decide Text () -> Mind ()
warnDecide m = either warn return =<< decide m

-- | Run an EitherT monad and print an error if `left'.
erroDecide :: Decide Text () -> Mind ()
erroDecide m = either erro return =<< decide m

-- }}}

-- {{{ Exception utils

try :: MonadIO m => IO a -> m (Either SomeException a)
try = liftIO . E.try

-- }}}

-- {{{ Funcs utils

-- | Try to read a storage file then return the Maybe result.
readConf :: (MonadIO m, Read a) => FilePath -> m (Maybe a)
readConf path = liftIO $ do
    ms <- fmap hush . try $ readFile path
    return $! join $ readMay <$> ms

-- Monoid or Maybe?
-- | Read a local (to the server and channel) value stored in a file in the
--   bot's path.
readLocalStored :: (Read a) => FilePath -> Mind (Maybe a)
readLocalStored path = do
    serv <- sees $ stServHost . currServ
    edest <- sees currDest
    dir <- stConfDir <$> readConfig
    mservers <- readConf $ dir <> path
    let dest = either userName stChanName edest
        mchans = join $ M.lookup serv <$> mservers
        mstoreds = join $ M.lookup dest <$> mchans
    return $ mstoreds

-- | Modify a local (to the server and channel) value stored in a file in the
--   bot's path.
--modLocalStored :: (Read a, Show b, Monoid a) =>
--                  FilePath -> (a -> b) -> Mind ()
modLocalStored path f = do
    serv <- sees $ stServHost . currServ
    edest <- sees currDest
    dir <- fmap stConfDir readConfig
    mservers <- readConf $ dir <> path
    let dest = either userName stChanName edest
        mchans = join $ M.lookup serv <$> mservers
        mstoreds = join $ M.lookup dest <$> mchans
        storeds = maybe (f mempty) f mstoreds
        chans = maybe (M.singleton dest storeds) (M.insert dest storeds) mchans
        servers = maybe (M.singleton serv chans) (M.insert serv chans) mservers
    e <- writeConf (dir <> path) servers
    either warn return e

writeConf :: (MonadIO m, Show a) => FilePath -> a -> m (Either Text ())
writeConf path a = liftIO $ do
    ms <- fmap louder . try $ writeFileSafe path $ T.pack $ show a
    return $! ms
  where
    louder :: Either SomeException a -> Either Text a
    louder (Left e) = Left $ T.pack $ show e
    louder (Right a) = Right a

-- TODO
colourise :: Text -> Text
colourise t = foldr ($) t (openers ++ closers)
  where
    opener c = T.replace (T.pack [c]) (T.snoc "\ETX10" c)
    closer c = T.replace (T.pack [c]) (T.cons c "\ETX")
    openers = map opener "[("
    closers = map closer "])"

-- TODO
-- | Get the API key for a service.
getAPIKey :: Text -> Mind (Maybe Text)
getAPIKey t = do
    dir <- stConfDir <$> readConfig
    keys <- readConf $ dir <> "api"
    return . join $ M.lookup t <$> keys

-- }}}

-- {{{ Funks utils

-- FIXME Funk max should be customizable.
funky :: Funky a -> Mind a
funky m = do
    s <- get
    fmap fst . runStateT m $ StFunk 0 39

allfuncs :: Mind Funcs
allfuncs = do
    funcs <- stConfFuncs <$> readConfig
    mlfuncs <- readLocalStored "letfuncs"
    let meval = (\f -> \g -> f . (g <>)) . funkFunc <$> M.lookup "eval" funcs
    if isJust meval
    then let eval = fromJust meval
             lfuncs = M.mapWithKey (\k v -> Funk k (eval v) Online)
         in return $ M.union funcs $ maybe mempty lfuncs mlfuncs
    else return funcs

-- }}}

-- {{{ HTTP utils

-- TODO
-- |
httpGetResponse' :: MonadIO m => String -> m (String, [(CI String, String)], String, String)
httpGetResponse' url = liftIO $ withManager $ \man -> do
    initReq <- parseUrl url
    let req = initReq { requestHeaders = useragent : requestHeaders initReq
                      }
    r <- httpLbs req man
    let b = responseBody r
        s = statusCode $ responseStatus r
        v = responseVersion r
        h = responseHeaders r
    liftIO $ closeManager man
    return $ ( BU.toString $ B.concat $ BL.toChunks b
             , flip map h $ \(k, v) ->
                (CI.map BU.toString k, BU.toString v)
             , show s
             , show v
             )
  where
    useragent = ( "User-Agent"
                , "Mozilla/5.0 (X11; Linux x86_64; rv:10.0.2) Gecko/20100101 Firefox/10.0.2"
                )

httpGetResponse :: MonadIO m => String -> m (String, [(CI String, String)], String, String)
httpGetResponse url = do
    let tryGet = liftIO $ do
            mr <- timeout (2 * 10 ^ 7) (try $ httpGetResponse' url)
            return $ maybe (Left Nothing) (either (Left . Just) Right) mr
    e <- tryGet
    case e of
        Right v -> return v
        Left Nothing -> do
            liftIO $ putStrLn "httpGetResponse: Connection timed out."
            return ([],[],[],[])
        Left (Just e) -> do
            liftIO $ putStr "httpGetResponse: " >> print e
            return ([],[],[],[])


httpGetString :: MonadIO m => String -> m String
httpGetString url = liftIO $  do
    (str, _, _, _) <- httpGetResponse url
    if length str > 10 ^ 6 * 2
        then return ""
        else return str

-- | No redirects or anything, also just the headers.
httpHead :: MonadIO m => String -> m (Map (CI String) String)
httpHead url = liftIO $ do
    e <- E.try $ withManager $ \man -> do
        initReq <- parseUrl url
        let req = initReq { requestHeaders = useragent : requestHeaders initReq
                          , redirectCount = 0
                          }
        r <- httpLbs req man
        let h = responseHeaders r
        liftIO $ closeManager man
        return $ M.fromList $ flip map h $ \(k, v) ->
            (CI.map BU.toString k, BU.toString v)
    case e of
        Right a -> return a
        Left (StatusCodeException _ headers _) ->
            return $ M.fromList $ flip map headers $ \(k, v) ->
                (CI.map BU.toString k, BU.toString v)
        Left _ -> return mempty
  where
    useragent = ( "User-Agent"
                , "Mozilla/5.0 (X11; Linux x86_64; rv:10.0.2) Gecko/20100101 Firefox/10.0.2"
                )

-- | Google translate
gtranslate :: MonadIO m => String -> String -> String
           -> m (Maybe ([[Text]], (Text, [Text])))
gtranslate sl tl q = do
    let url = concat [ "http://translate.google.com"
                     , "/translate_a/t"
                     , "?client=t&hl=en"
                     , "&sl=" <> sl <> "&tl=" <> tl <> "&q=" <> urlEncode q
                     ]
    jsonStr <- httpGetString $ url
    let m = A.maybeResult $ A.parse transparser (T.pack jsonStr) `A.feed` ""
    unless (isJust m) $ warn jsonStr
    return m

-- }}}

-- {{{ IO utils

-- | Append to a temporary file and move it to the intended location.
appendFileSafe :: MonadIO m => FilePath -> Text -> m ()
appendFileSafe p t = liftIO $ do
    eo <- try $ T.readFile p
    o <- either (mvoid . warn) return eo
    writeFileSafe p $ o <> t

-- | Write to a temporary file and move it to the intended location.
writeFileSafe :: MonadIO m => FilePath -> Text -> m ()
writeFileSafe p t = liftIO $ do
    (tp, th) <- openTempFile "/tmp" "append"
    T.hPutStr th t
    hClose th
    copyFile tp p
    removeFile tp

-- }}}

-- {{{ IRC Text utils

ctcp :: Text -> Text
ctcp t = "\SOH" <> t <> "\SOH"

-- | Is it a channel?
isChan :: Text -> Bool
isChan = T.isPrefixOf "#"

-- | From symbols to MODE characters.
toMode :: [Char] -> [Char]
toMode t = map modeChar t
  where
    modeChar '~' = 'q'
    modeChar '&' = 'a'
    modeChar '@' = 'o'
    modeChar '%' = 'h'
    modeChar '+' = 'v'

-- | Is the MODE +a +o or +q?
isMod :: [Char] -> Bool
isMod = any (`elem` "oaq")

-- | Join several messages with "|" and only until 420 chars.
pipeJoin :: [Text] -> (Maybe Text, [Text])
pipeJoin [] = (Nothing, [])
pipeJoin (x:[]) = (Just x, [])
pipeJoin (x:y:z:xs) | T.length (x <> y <> z) < 420 =
    let x' = Just $ x <> " | " <> y <> " | " <> z
    in (x', xs)
pipeJoin (x:y:xs) | T.length (x <> y) < 420 = (Just $ x <> " | " <> y, xs)
                  | otherwise = (Just x, y:xs)

-- }}}

-- {{{ IRC Mind utils

-- | Adapt the Current to the channel.
adaptWith :: Text -> Nick -> Text -> Text
          -> (User -> (Users -> Users)) -> Mind ()
adaptWith chan nick name host f = do
    mchan <- sees $ M.lookup chan . stServChans . currServ
    users <- sees $ stServUsers . currServ
    let muser = M.lookup nick users
        puser = fromJust $ muser <|> Just (User nick name host Online mempty)
        chans = if isChan chan
                then M.alter (maybe (Just "") Just) chan $ userChans puser
                else userChans puser
        user = puser { userName = name
                     , userHost = host
                     , userChans = chans
                     }
        dchan = defStChan { stChanName = chan }
        edest = if isChan chan
                then Right . fromJust $ mchan <|> Just dchan
                else Left user
    sets $ \c -> c { currDest = edest, currUser = user }
    modUserlist $ f user

-- }}}

-- {{{ JSON utils

resultMay (Ok x) = Just x
resultMay _ = Nothing

objectMay (JSObject x) = Just x
objectMay _ = Nothing

stringMay (JSString x) = Just x
stringMay _ = Nothing

arrayMay (JSArray x) = Just x
arrayMay _ = Nothing

boolMay (JSBool x) = Just x
boolMay _ = Nothing

intMay (JSRational _ x) = Just $ round x
intMay _ = Nothing

floatMay (JSRational _ x) = Just $ fromRational x
floatMay _ = Nothing

-- }}}

-- {{{ Maybe utils

-- | Maybe to any monoid value.
maybeToMonoid :: Monoid a => Maybe a -> a
maybeToMonoid (Just x) = x
maybeToMonoid Nothing = mempty

mayT :: Monad m => Maybe a -> MaybeT m a
mayT = maybe (fail "") return

-- }}}

-- {{{ Mind utils

-- XXX Do we make functions that use warn and verb, or do we use a verbosity
--     checking function, comparable to `when', except only taking one argument
-- - We should move the verbosity utils elsewhere; Debug utils?

-- TODO check verbosity
warn :: (MonadIO m, Show a) => a -> m ()
warn x = liftIO $ putStrLn $ "\x1b[0;33mWarning " <> show x <> "\x1b[0m"

-- TODO check verbosity
verb :: (MonadIO m, Show a) => a -> m ()
verb x = liftIO $ putStrLn $ "\x1b[1;33mVerbose " <> show x <> "\x1b[0m"

-- TODO check verbosity
erro :: (MonadIO m, Show a) => a -> m ()
erro x = liftIO $ putStrLn $ "\x1b[0;31mError " <> show x <> "\x1b[0m"

-- | Put a message to the current `Server'\'s `Handle'.
write :: Text -> Mind ()
write t = do
    h <- sees currHandle
    ex <- liftIO $ try $ do
        T.hPutStrLn h t
        T.putStrLn $ "\x1b[0;32m" <> t <> "\x1b[0m"
    either warn return ex

-- | Put a private message using `write'
putPrivmsg :: Text -> Text -> Mind ()
putPrivmsg d t = unless (T.null t) $ do
    let t' = if T.length t > 420
             then T.take 420 t <> "…"
             else t
    write $ "PRIVMSG " <> d <> " :" <> t'

-- | Fork for the Mind monad
forkMi :: Mind () -> Mind ThreadId
forkMi m = do
    s <- get
    liftIO . forkIO . void $ runStateT m s

-- |
noNumeric :: Text -> Mind ()
noNumeric n = warn $ n <> ": No Numeric argument"

-- }}}

-- {{{ Monoid utils

-- | Equivalent of `void' returning `mempty' instead of `()'.
mvoid :: (Monoid b, Monad m) => m a -> m b
mvoid m = m >> return mempty

-- | When `True', run the monadic action `m', otherwise return mempty in the
--   current monad.
mwhen :: (Monoid a, Monad m) => Bool -> m a -> m a
mwhen True m = m
mwhen False _ = return mempty

munless :: (Monoid a, Monad m) => Bool -> m a -> m a
munless b = mwhen $ not b

-- | Join until predicate `p' is `True' on the joined list and then return the
--   list as it was just before `True' was returned.
--
-- > joinUntil ((>= 15) . length) ", " ["Banana", "Apple", "Pear"]
-- > "Banana, Apple"
joinUntil :: Monoid a => (a -> Bool) -> a -> [a] -> a
joinUntil _ _ [] = mempty
joinUntil p _ [x] = if p x then mempty else x
joinUntil p str (x:strs) = joinUntil' p x str strs
  where
    joinUntil' p acc str [] = acc
    joinUntil' p acc str (x:[]) = let acc' = acc <> x
                                  in if p acc' then acc else acc'
    joinUntil' p acc str (x:xs) = let acc' = acc <> str <> x
                                  in if p acc'
                                     then acc
                                     else joinUntil' p acc' str xs

-- }}}

-- {{{ StateT utils

-- |
set :: Current -> Mind ()
set c = do
    t <- get
    void . liftIO . atomically $ swapTMVar t c

-- |
sets :: (Current -> Current) -> Mind ()
sets f = do
    t <- get
    void . liftIO . atomically $ do
        v <- readTMVar t
        swapTMVar t $ f v

-- |
see :: Mind Current
see = get >>= \c -> liftIO $! atomically $! readTMVar $! c

-- |
sees :: (Current -> a) -> Mind a
sees f = fmap f $! get >>= \c -> liftIO $! atomically $! readTMVar $! c

-- }}}

-- {{{ Text utils

-- | Break on True for `p' and split the matching words from the non-matching.
wordbreaks :: (Text -> Bool) -> Text -> ([Text], [Text])
wordbreaks p t = foldr f ([], []) $ T.words t
  where
    f x (lacc, racc) | p x = (lacc, x : racc)
                     | otherwise = (x : lacc, racc)

-- | Replace the first argument using a dictionary where fst is the match and
-- snd is the replacement. This is semi case insensitive and the result will
-- match the case. It also only affects alphabetical characters and any other
-- results in the word will be kept in the result. A word counts as any
-- characters leading up to any non-alphabetical character.
--
-- `wordReplace "banana!banana" [("banana", "apple")] == "apple!banana"`
-- `wordReplace "Banana! banana!" [("banana", "apple")] == "Apple! apple!"`
-- `wordReplace "BANANA" [("banana", "apple")] == "APPLE"`
wordReplace :: Text -> [(Text, Text)] -> Text
wordReplace str bs = T.unwords $ foldr (replacer bs) [] $ T.words str
  where
    replacer bs x acc
        | T.null x = x : acc
        | otherwise =
            let (word, rest) = T.break notAlphabet x
                mws = do
                    (ma, re) <- bs
                    return $ do
                        f <- lookup word (capFunc ma)
                        return $ f re <> rest
                mx' = join (listToMaybe $ filter isJust mws) <|> Just x
            in fromJust mx' : acc
    capFunc x =
        let low = T.toLower
            headUp t = toUpper (T.head t) `T.cons` T.tail t
            up = T.toUpper
        in [ (low x, low)
           , (headUp x, headUp)
           , (up x, up)
           ]
    notAlphabet = flip notElem $ ['a' .. 'z'] ++ ['A' .. 'Z']

bisect p = fmap tail' . T.break p
  where
    tail' "" = ""
    tail' t = T.tail t

-- }}}

-- {{{ TMVar utils

mapTMVar :: (a -> a) -> TMVar a -> STM ()
mapTMVar f t = do
    a <- takeTMVar t
    putTMVar t $ f a

readConfig :: Mind StConfig
readConfig = do
    configt <- sees currConfigTMVar
    liftIO $ atomically $ readTMVar configt

mapConfig :: (StConfig -> StConfig) -> Mind ()
mapConfig f = do
    configt <- sees currConfigTMVar
    liftIO $ atomically $ mapTMVar f configt

-- }}}

-- {{{ Tuple utils

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

-- }}}

-- {{{ XML utils

-- XXX this is a UGLY AS FLIP
-- - We can use `join . fmap f' and keep things safe instead of using default
--   values and error.

-- | Get `Element's from contents
contentsToEls :: [Text.XML.Light.Content] -> [Element]
contentsToEls xs =
    let f (Elem x) acc = x : acc
        f _ acc = acc
    in foldr f [] xs

-- | Recursively gets text of element and all children and handles <br>
-- elements as spaces instead.
elemsText :: Element -> String
elemsText e =
    let name = elName e
        attrs = elAttribs e
        content = elemsText' . elContent $ e
        line = elLine e
        elem' = Element name attrs [content] line
    in strContents elem'
  where elemsText' :: [Text.XML.Light.Content] -> Text.XML.Light.Content
        elemsText' cs =
            let f x acc = case () of
                  _ | isElem x -> if isBR $ fromElem x
                        then " | " : (fold . elContent . fromElem) x ++ acc
                        else (fold . elContent . fromElem) x ++ acc
                    | isText x -> textToString x : acc
                    | otherwise -> acc
                fold x = foldr f [] x
            in genText . concat . fold $ cs
        isText (Text _) = True
        isText _ = False
        isElem (Elem _) = True
        isElem _ = False
        isBR (Element (QName n _ _) _ _ _) =
            n `elem` ["br", "bR", "Br", "BR"]
        isBR _ = False
        fromElem (Elem x) = x
        textToString (Text x) = cdData x
        genText x = Text $ CData { cdVerbatim = CDataText
                                 , cdData = x
                                 , cdLine = Nothing
                                 }

-- | Find elements by attributes
findElementsAttrs :: QName -> [Attr] -> Element -> [Element]
findElementsAttrs name attrs element = filterElements match element
  where match :: Element -> Bool
        match (Element name' attrs' _ _) =
            if qName name == qName name' || null (qName name) then
                if attrs `compare_` attrs'
                    then True
                    else False
            else False
        compare_ x y =
            let f x' acc | x' `elem` y = (True, x') : acc
                         | otherwise = (False, x') : acc
            in and $ map fst $ foldr f [] x

-- | Find first element by attributes
findElementAttrs :: QName -> [Attr] -> Element -> Maybe Element
findElementAttrs name attrs element =
    listToMaybe $ findElementsAttrs name attrs element

-- | Find elements inside an element
findElementsIn :: QName -> Element -> [Element]
findElementsIn q e =
    let elems = (contentsToEls . elContent) e
    in join $ map (findElements q) elems

-- | Safe from maybe to element
fromMaybeElement :: Maybe Element -> Element
fromMaybeElement (Just a) = a
fromMaybeElement Nothing  = Element (QName "" Nothing Nothing) [] [] Nothing

-- | Unsafe from Text to CData
fromText :: Text.XML.Light.Content -> CData
fromText (Text a) = a
fromText _        = error "Not `Text'"

-- | Get the text from contents of an element
strContents (Element _ _ content _) =
    unwords $ map (cdData . fromText) . filter isText $ content
  where isText (Text _) = True
        isText _ = False

-- }}}

