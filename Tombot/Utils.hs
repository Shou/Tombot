
-- This file is part of Tombot, licensed under the GNU GPL 2 license.
-- See the file "LICENSE" for more information.
-- Copyright Shou, 2013

{-# LANGUAGE OverloadedStrings, TupleSections, DoAndIfThenElse,
             TypeFamilies, TypeOperators, OverloadedLists,
             ScopedTypeVariables, PartialTypeSignatures,
             AllowAmbiguousTypes, TypeApplications, FlexibleContexts,
             DataKinds, BangPatterns
#-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Tombot.Utils where

-- {{{ Imports
import Tombot.Types
import Tombot.Connection

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Error
import Control.Exception (SomeException)
import qualified Control.Exception as E
import Control.Lens as Lens hiding (sets, inside)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except (ExceptT(..))
import Control.Type.Operator

import qualified Data.Aeson as Aes
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BU
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Char
import Data.Default
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as TextErr
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Encoding as TextLazy
import Data.Vector (Vector)
import qualified Data.Vector as Vec

import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite3 as SQL ( backupInit
                                         , backupStep
                                         , backupRemaining
                                         , backupFinish
                                         )

import Network.HTTP (urlEncode)
import Network.HTTP.Types.Status
import qualified Network.Wreq as Wreq

import System.Directory (copyFile, removeFile, getTemporaryDirectory)
import System.FilePath.Posix ((</>), dropDrive)
import System.IO (hClose, openTempFile, Handle)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

-- }}}

version :: Text
version = "Tombot 0.3.0 (i'm not a boy)"

-- {{{ Attoparsec utils

-- | manyTill but return the end result as well.
manyTillKeep :: Parser a -> Parser b -> Parser ([a], b)
manyTillKeep p end = scan ([], undefined)
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
    text <- Text.unpack <$> (A.try A.space >> A.takeText) <|> return ""
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
    text <- Text.unpack <$> (A.try A.space >> A.takeText) <|> return ""
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
        t <- Text.pack <$> inside '"'
        A.char '"'
        return t

-- }}}

-- {{{ Config utils

-- | `either' function for `Allowed' data.
allow :: (a -> b) -> (a -> b) -> Allowed a -> b
allow f _ (Blacklist a) = f a
allow _ g (Whitelist a) = g a

-- | Get the `Channel'.
getChan :: Text -> Mind s (Maybe $ Channel s)
getChan chan = do
    server <- sees $ view currServer
    return $ Map.lookup chan $ view servChannels server

-- | Get a Channel record field.
getChanField :: Text -> (Channel s -> a) -> Mind s (Maybe a)
getChanField chan f = do
    channel <- getChan chan
    return $ fmap f channel

-- | Get User by their user id
getUser :: Text -> Mind s (Maybe $ User s)
getUser nick = sees _currServer >>= return . Map.lookup nick . view servUsers

mapChans :: (Set $ CI Text -> Set $ CI Text) -> User s -> User s
mapChans f = over userChannels f

mapStat :: (UserStatus -> UserStatus) -> User s -> User s
mapStat f = over userStatus f

-- | Modify the server's userlist.
modUserlist f = do
    s <- sees $ view currServer
    sets $ over currServer $ over servUsers f

-- | Modify `Channel' data.
modChan :: Text -> (Channel s -> Channel s) -> Mind s (Either Text ())
modChan chan f = do
    server <- sees _currServer
    let cs = _servChannels server
        mc = Map.lookup chan cs
        ec = note ("No channel: " <> chan) mc
        cs' = maybe cs (flip (Map.insert chan) cs . f) mc
    sets $ over currServer $ over servChannels (const cs')
    return $ void ec

-- | Change the topic of a channel.
modChanTopic :: Text -> (Text -> Text) -> Mind s (Either Text ())
modChanTopic chan f = modChan chan $ over chanTopic f

-- | Change the Funcs of a channel.
modChanFuncs :: Text -- ^ Channel
             -> (Map Text $ Funk s -> Map Text $ Funk s)
             -> Mind s (Either Text ())
modChanFuncs chan f = modChan chan $ over chanFuncs f

-- | Change whether the bot is allowed to join a channel.
modChanJoin :: Text -> (Bool -> Bool) -> Mind s (Either Text ())
modChanJoin chan f = modChan chan $ over chanJoin f

-- | Change whether the bot should auto-join on reconnect/kick.
modChanAutoJoin :: Text -> (Bool -> Bool) -> Mind s (Either Text ())
modChanAutoJoin chan f = modChan chan $ over chanAutoJoin f

-- | Change the function prefix characters.
modChanPrefix :: Text -> ([Char] -> [Char]) -> Mind s (Either Text ())
modChanPrefix chan f = modChan chan $ over chanPrefix f

-- | Modify User data.
modUser :: Text -> (User s -> User s) -> Mind s (Either Text ())
modUser user f = do
    server <- sees _currServer

    let us = _servUsers server
        mu = Map.lookup user us
        eu = note ("No user: " <> user) mu
        us' = maybe us (flip (Map.insert user) us . f) mu

    sets $ over currServer $ over servUsers (const us')
    return $ void eu

-- | When predicate `p' is True, run `m', otherwise return `()'.
whenStat :: (Either [Char] UserStatus -> Bool) -> Mind s () -> Mind s ()
whenStat p m = do
    dest <- either _userId _chanId <$> sees _currDestination
    chans <- sees $ _userChannels . _currUser
    stat <- sees $ _userStatus . _currUser
    let isMode = False -- maybe False (p . Left) $ Map.lookup dest chans
        isStat = p $ Right stat
    case () of
      _ | isMode -> m
        | isStat -> m
        | otherwise -> return ()

-- | When predicate `p' is True, run `m', otherwise return `mempty'.
mwhenStat :: Monoid a => (Either [Char] UserStatus -> Bool) -> Mind s a -> Mind s a
mwhenStat p m = do
    dest <- either _userId _chanId <$> sees _currDestination
    chans <- sees $ _userChannels . _currUser
    stat <- sees $ _userStatus . _currUser
    let isMode = False -- maybe False (p . Left) $ Map.lookup dest chans
        isStat = p $ Right stat
    case () of
      _ | isMode -> m
        | isStat -> m
        | otherwise -> return mempty

mwhenPrivileged :: Monoid a => Mind s a -> Mind s a
mwhenPrivileged = mwhenStat (either isMod (>= Mod))

mwhenUserStat :: Monoid a => (UserStatus -> Bool) -> Mind s a -> Mind s a
mwhenUserStat p = mwhenStat (either (const False) p)

mwhenPrivTrans :: Monoid a => UserStatus -> Mind s a -> Mind s a
mwhenPrivTrans u = mwhenStat (either isPriv (>= u))
  where
    isPriv = if u >= Mod then isMod else const False

unlessBanned :: Mind s () -> Mind s ()
unlessBanned m = whenStat (either (const False) (/= Banned)) m

-- | From a list of tri-tuple funcs to Funks
toFunks :: [(Text, Text -> Mind s Text, UserStatus)] -> Map Text $ Funk s
toFunks = Map.fromList . map (\(n, f, s) -> (n, Funk n f s))

seeConfig :: Mind s Config
seeConfig = _currConfig <$> see

-- }}}

-- {{{ Decide utils

-- | Run an ExceptT monad inside Mind.
decide :: Decide s e a -> Mind s (Either e a)
decide m = runExceptT m

-- | Run an ExceptT monad and print a warning if `left'.
warnDecide :: Decide s Text () -> Mind s ()
warnDecide m = either warn return =<< decide m

-- | Run an ExceptT monad and print an error if `left'.
erroDecide :: Decide s Text () -> Mind s ()
erroDecide m = either erro return =<< decide m

-- }}}

-- {{{ Exception utils

try :: MonadIO m => IO a -> m (Either SomeException a)
try = liftIO . E.try

-- }}}

-- {{{ Funcs utils

readAeson :: (MonadIO m, Aes.FromJSON a) => FilePath -> m $ Maybe a
readAeson = liftIO . fmap (join . fmap Aes.decode . hush)
          . try . BSL.readFile

-- | Try to read a storage file then return the Maybe result.
readConfig :: (MonadIO m, Read a) => FilePath -> m (Maybe a)
readConfig path = liftIO $ do
    ms <- fmap hush . try $ readFile path
    return $! join $ readMay <$> ms

-- Monoid or Maybe?
-- | Read a local (to the server and channel) value stored in a file in the
--   bot's path.
readLocalStored :: (Read a) => FilePath -> Mind s (Maybe a)
readLocalStored path = do
    mchans <- readServerStored path
    edest <- sees _currDestination

    let dest = either _userId _chanId edest
        mstoreds = join $ Map.lookup dest <$> mchans

    return mstoreds

-- | Read a local (to the server) value stored in a file in the bot's path.
readServerStored :: (Read a) => FilePath -> Mind s (Maybe a)
readServerStored path = do
    !serv <- sees $ _servId . _currServer
    !dir <- _confDirectory <$> fmap _currConfig see
    mservers <- readConfig $ dir </> path

    let mchans = join $ Map.lookup serv <$> mservers

    return mchans

-- | Return a list of all global 'stored' values
readGlobalStored :: (IsString k, Ord k, Read k, Read a)
                 => FilePath -> Mind s $ Map k a
readGlobalStored path = do
    !dir <- _confDirectory <$> fmap _currConfig see

    mservers <- readConfig @_ @(Map Text $ Map Text _) $ dir </> path

    let unionChans = Map.unions . Map.elems <$> mservers
        unionStoreds = Map.unions . Map.elems <$> unionChans

    return $ maybe Map.empty id unionStoreds

mapConfig :: (Config -> Config) -> Mind s ()
mapConfig = sets . over currConfig

-- | Modify a local (to the server and channel) value stored in a file in the
--   bot's path.
--modLocalStored :: (Read a, Show b, Monoid a) =>
--                  FilePath -> (a -> b) -> Mind s ()
modLocalStored path f = do
    serv <- sees $ _servId . _currServer
    edest <- sees _currDestination
    dir <- _confDirectory <$> seeConfig
    mservers <- readConfig $ dir </> path

    let dest = either _userId _chanId edest
        mchans = join $ Map.lookup serv <$> mservers
        mstoreds = join $ Map.lookup dest <$> mchans
        storeds = maybe (f mempty) f mstoreds
        chans = maybe (Map.singleton dest storeds) (Map.insert dest storeds) mchans
        servers = maybe (Map.singleton serv chans) (Map.insert serv chans) mservers

    e <- writeConf (dir </> path) servers

    either warn return e

writeConf :: (MonadIO m, Show a) => FilePath -> a -> m (Either Text ())
writeConf path a = liftIO $ do
    ms <- fmap louder . try $ writeFileSafe path $ Text.pack $ show a
    return $! ms
  where
    louder :: Either SomeException a -> Either Text a
    louder (Left e) = Left $ Text.pack $ show e
    louder (Right a) = Right a

-- TODO
colourise :: Text -> Text
colourise t = foldr ($) t (openers ++ closers)
  where
    opener c = Text.replace (Text.pack [c]) (Text.snoc "\ETX10" c)
    closer c = Text.replace (Text.pack [c]) (Text.cons c "\ETX")
    openers = map opener "[("
    closers = map closer "])"

-- TODO
-- | Get the API key for a service.
getAPIKey :: (Read str, IsString str) => Text -> Mind s (Maybe str)
getAPIKey t = do
    dir <- _confDirectory <$> seeConfig
    apiKeys <- readConfig $ dir </> "api"
    return . join $ Map.lookup t <$> apiKeys

-- }}}

-- {{{ Funks utils

-- FIXME Funk max should be customizable.
funky :: Funky s a -> Mind s a
funky m = fmap fst . runStateT m $ StFunk 0 1000

serverfuncs :: Map Text $ Funk s -> Mind s $ Map Text $ Funk s
serverfuncs funcs = do
    globalFuncs <- readGlobalStored "letfuncs"
    mschans <- readServerStored "letfuncs"
    edest <- sees _currDestination

    let serverFuncs = maybe mempty id $ Map.unions . Map.elems <$> mschans
        dest = either _userId _chanId edest
        localFuncs = maybe mempty id $ join $ Map.lookup dest <$> mschans
        allFuncs = localFuncs <> serverFuncs <> globalFuncs
        meval = (\f -> \g -> f . (g <>)) . funkFunc <$> Map.lookup "eval" funcs

    if isJust meval
    then let eval = fromJust meval
             allFuncs' = Map.mapWithKey (\k v -> Funk k (eval v) Online) allFuncs
         in return allFuncs'

    else return mempty

localfuncs :: Mind s $ Map Text $ Funk s
localfuncs = do
    return mempty

-- }}}

-- {{{ HTTP utils

wreqOpts = Wreq.defaults
         & Wreq.header "User-Agent" .~ [mozUserAgent]

mozUserAgent =  "Mozilla/5.0 (X11; Linux x86_64; rv:10.0) Gecko/20100101 Firefox/10.0"

httpGetResponse :: MonadIO m => String
                 -> m (String, [(CI String, String)], String)
httpGetResponse url = liftIO $ do
    er <- try $ Wreq.getWith wreqOpts url

    flip (either $ const $ return ("", [], "")) er $ \r -> do
        let mbody = fmap toString $ r ^? Wreq.responseBody
            body = mayempty mbody
            mstatus = fmap show (r ^? Wreq.responseStatus) <> Just " \x2014 "
            status = mayempty mstatus
            bheaders = r ^. Wreq.responseHeaders
            headers = []

        return (body, headers, status)
  where
    toString = BU.toString . BSL.toStrict
    mayempty :: Monoid a => Maybe a -> a
    mayempty = maybe mempty id

httpGetString :: MonadIO m => String -> m String
httpGetString url = liftIO $ do
    str <- view _1 <$> httpGetResponse url
    mwhen (length str < 10 ^ 6 * 2) $ return str

-- | No redirects or anything, also just the headers.
httpHead :: MonadIO m => String -> m (Map (CI String) String)
httpHead url = Map.fromList . view _2 <$> httpGetResponse url

-- | Google translate
googleTranslate :: MonadIO m => String -> String -> String
           -> m (Maybe ([[Text]], (Text, [Text])))
googleTranslate sl tl q = do
    let url :: String
        url = mconcat
          [ "https://translate.googleapis.com"
          , "/translate_a/single"
          , "?client=gtx"
          , "&sl=", sl, "&tl=", tl, "&q=", urlEncode q 
          ]

        -- OLD URL
--        ourl =
--          [ "http://translate.google.com"
--          , "/translate_a/t"
--          , "?client=t&hl=en"
--          , "&sl=" <> sl <> "&tl=" <> tl <> "&q=" <> urlEncode q
--          ]

    request <- try $ Wreq.getWith wreqOpts url

    let jsonText = request ^. _Right . Wreq.responseBody
                 . Lens.to BSL.toStrict
                 . Lens.to (Text.decodeUtf8With TextErr.lenientDecode)
        m = A.maybeResult $ A.parse transparser jsonText `A.feed` ""

    unless (isJust m) $ warn jsonText

    return m

-- }}}

-- {{{ IO utils

-- | Append to a temporary file and move it to the intended location.
appendFileSafe :: MonadIO m => FilePath -> Text -> m ()
appendFileSafe p t = liftIO $ do
    eo <- try $ Text.readFile p
    o <- either (mvoid . warn) return eo
    writeFileSafe p $ o <> t

-- | Write to a temporary file and move it to the intended location.
writeFileSafe :: MonadIO m => FilePath -> Text -> m ()
writeFileSafe p t = liftIO $ do
    tmpDir <- getTemporaryDirectory
    (tp, th) <- openTempFile tmpDir "append"
    Text.hPutStr th t
    hClose th
    copyFile tp p
    removeFile tp

-- }}}

-- {{{ Database utils

{-# NOINLINE memoryDBRef #-}
memoryDBRef = unsafePerformIO $ SQL.open ":memory:" >>= newTMVarIO

memoryDB :: MonadIO m => m SQL.Connection
memoryDB = liftIO $ atomically $ readTMVar memoryDBRef

{-# NOINLINE fileDBRef #-}
fileDBRef = unsafePerformIO $ SQL.open "db" >>= newTMVarIO

fileDB :: MonadIO m => m SQL.Connection
fileDB = liftIO $ atomically $ readTMVar fileDBRef

-- TODO per-channel logs
initDB :: MonadIO m => m ()
initDB = liftIO $ do
    mdb <- memoryDB
    fdb <- fileDB
    let q = "CREATE TABLE IF NOT EXISTS logs("
         <> "date TEXT NOT NULL,"
         <> "mesg TEXT NOT NULL,"
         <> "nick TEXT NOT NULL,"
         <> "chnl TEXT NOT NULL,"
         <> "iden TEXT);"
    forM_ @[] [mdb, fdb] $ flip SQL.execute_ q

insertLog date mesg nick chnl iden = liftIO $ do
    mdb <- memoryDB
    fdb <- fileDB
    forM_ @[] [mdb, fdb] $ \db -> do
        let query = "INSERT INTO logs (date, mesg, nick, chnl, iden) VALUES (?,?,?,?,?)"
        SQL.execute db query (date, mesg, nick, chnl, iden)

queryLogs :: (MonadIO m, SQL.FromRow a) => m [a]
queryLogs = liftIO $ do
    db <- memoryDB
    SQL.query_ db "SELECT * FROM logs;"

loadDB :: MonadIO m => m ()
loadDB = liftIO $ do
    mdb <- SQL.connectionHandle <$> memoryDB
    fdb <- SQL.connectionHandle <$> fileDB
    bu <- SQL.backupInit mdb "main" fdb "main"
    r <- SQL.backupStep bu (-1)
    step <- SQL.backupRemaining bu
    print r >> print step
    SQL.backupFinish bu

-- }}}

-- {{{ IRC Text utils

ctcp :: Text -> Text
ctcp t = "\SOH" <> t <> "\SOH"

-- | Is it a channel?
isChan :: Text -> Bool
isChan = Text.isPrefixOf "#"

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
isMod = any (`elem` (['o', 'a', 'q'] :: [Char]))

-- | Join several messages with "|" and only until 420 chars.
pipeJoin :: [Text] -> (Maybe Text, [Text])
pipeJoin [] = (Nothing, [])
pipeJoin (x:[]) = (Just x, [])
pipeJoin (x:y:z:xs) | Text.length (x <> y <> z) < 420 =
    let x' = Just $ x <> " | " <> y <> " | " <> z
    in (x', xs)
pipeJoin (x:y:xs) | Text.length (x <> y) < 420 = (Just $ x <> " | " <> y, xs)
                  | otherwise = (Just x, y:xs)

-- }}}

-- {{{ IRC Mind utils

-- | Adapt the Current s to the channel.
adaptWith :: forall s. Default <=> '[Channel s, User s]
          => Text -> Text -> Text -> Text -> Text
          -> (User s -> (Users s -> Users s)) -> Mind s ()
adaptWith chan nick uid name host f = do
    mchan <- sees $ Map.lookup chan . _servChannels . _currServer
    users <- sees $ _servUsers . _currServer

    let muser = Map.lookup uid users
        newUser = defUser & Lens.set userNick nick
                          . Lens.set userId uid
                          . Lens.set userName name

        puser = maybe newUser id muser
        user = puser & Lens.set userName name
        newChan = defChan & Lens.set chanName (CI.mk chan)
        edest = if isChan chan
                then Right $ maybe newChan id mchan
                else Left user

    sets $ Lens.set currDestination edest . Lens.set currUser user
    modUserlist $ f user
  where
    defChan :: Channel s
    defChan = def
    defUser :: User s
    defUser = def

-- }}}

-- {{{ Monad utils

scanM :: Monad m => (b -> a -> m b) -> b -> [a] -> m [b]
scanM f acc xs = sequence $ scanl g (return acc) xs
  where g b a = join $ liftM2 f b (return a)

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
write :: Text -> Text -> Mind s ()
write service text = do
    let mtqueue = Map.lookup service connections
    ex <- try $ flip (maybe noService) mtqueue $ \tqueue -> do
        atomically $ writeTQueue tqueue text
        Text.putStrLn $ "\x1b[0;32m" <> text <> "\x1b[0m"
    either warn return ex
  where
    noService = warn $ "Service " <> service <> " does not exist."

-- | Put a private IRC message using `write'
putPrivmsg :: Text -> Text -> Mind s ()
putPrivmsg d t = unless (Text.null t) $ do
    let t' = if Text.length t > 420
             then Text.take 420 t <> "…"
             else t
    write "IRC" $ "PRIVMSG " <> d <> " :" <> t'

-- | Fork for the Mind monad
forkMi :: Mind s () -> Mind s ThreadId
forkMi m = liftIO . forkIO . void . runReaderT m =<< ask

-- |
noNumeric :: Text -> Mind s ()
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

-- | Modifies the current shared state.
sets :: (Current s -> Current s) -> Mind s ()
sets f = void . liftIO . atomically . flip modifyTVar' f =<< ask

-- | Set and replace the current shared state.
set :: Current s -> Mind s ()
set = sets . const

-- | Retrieves the current shared state with a modifier function applied.
sees :: (Current s -> a) -> Mind s a
sees f = fmap f . liftIO . readTVarIO =<< ask

-- | Retrieves the current shared state.
see :: Mind s $ Current s
see = sees id

-- }}}

-- {{{ Text utils

ciEq a b = CI.mk a == CI.mk b

-- | Break on True for `p' and split the matching words from the non-matching.
wordbreaks :: (Text -> Bool) -> Text -> ([Text], [Text])
wordbreaks p t = foldr f ([], []) $ Text.words t
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
wordReplace str bs = Text.unwords $ foldr (replacer bs) [] $ Text.words str
  where
    replacer bs x acc
        | Text.null x = x : acc
        | otherwise =
            let (word, rest) = Text.break notAlphabet x
                mws = do
                    (ma, re) <- bs
                    return $ do
                        f <- lookup word (capFunc ma)
                        return $ f re <> rest
                mx' = join (listToMaybe $ filter isJust mws) <|> Just x
            in fromJust mx' : acc
    capFunc x =
        let low = Text.toLower
            headUp t = toUpper (Text.head t) `Text.cons` Text.tail t
            up = Text.toUpper
        in [ (low x, low)
           , (headUp x, headUp)
           , (up x, up)
           ]
    notAlphabet = flip notElem $ ['a' .. 'z'] ++ ['A' .. 'Z']

bisect p = fmap tail' . Text.break p
  where
    tail' "" = ""
    tail' t = Text.tail t

-- }}}

-- {{{ Tuple utils

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

-- }}}

-- {{{ Decision tree

-- XXX TODO split based on punctuation, e.g. commas and full stops,
--          except in cases like "e.g.", abbreviations.
--          This will detach punctuation from the `origIdiom`, and also
--          make it randomisable.
-- | Case-insensitive Text stripped of punctuation and symbols
data Idiom = Idiom { origIdiom :: Text, foldedIdiom :: CI Text }

instance Eq Idiom where
    (==) a b = foldedIdiom a == foldedIdiom b

instance Ord Idiom where
    compare a b = foldedIdiom a `compare` foldedIdiom b

instance IsString Idiom where
    fromString = mkIdiom . Text.pack

instance Show Idiom where
    show = show . origIdiom

instance Monoid Idiom where
    mempty = mkIdiom ""
    mappend (Idiom o1 f1) (Idiom o2 f2) = Idiom (mappend o1 o2) (mappend f1 f2)

mkIdiom :: Text -> Idiom
mkIdiom t = Idiom t (CI.mk $ stripGarbage t)

mapIdiom :: (Text -> Text) -> Idiom -> Idiom
mapIdiom f = mkIdiom . f . origIdiom

stripGarbage :: Text -> Text
stripGarbage = Text.filter p
  where
    p = and . zipWith ($) ps . repeat
    ps = [ \c -> not (isPunctuation c) || c == '\''
         , not . isSymbol
         ]

-- XXX contractions: U.K., e.g., Will Text. Smith, Cont. next page
--     numbers: 4.5, 4,000,000
-- TODO
tokenizer :: Parser [Text]
tokenizer = do
    tokens <- A.many1' $ A.choice tokParsers

    return tokens
  where
    tokParsers = [rational, spaces]
    spaces = A.takeWhile1 isSpace
    rational = do
        n <- A.takeWhile1 isDigit
        A.char '.'
        m <- A.takeWhile1 isDigit
        return $ n <> "." <> m

-- | Make a 'corpus': a map of words and their associated quantities
--      - Quantity of messages the word is contained within
--      - Total quantity: how many times the word appears anywhere
--        including its own message
makeCorpus :: [Text] -> Map Idiom (Int, Int)
makeCorpus = flip appendCorpus Map.empty

-- TODO review this
-- | Append to an existing corpus
appendCorpus :: [Text] -> Map Idiom (Int, Int) -> Map Idiom (Int, Int)
appendCorpus msgs cs = Map.unionsWith (\(an, ap) (bn, bp) -> (an+bn, ap+bp)) $ map f msgs
  where
    f msg =
        let is = map mkIdiom $ Text.words msg
        in foldr (Map.alter (Just . maybe (1, 1) (_2 +~ 1))) cs is

getPredictionTree :: IO $ Map Idiom $ Vector $ Map Idiom Int
getPredictionTree = do
    (rls :: [(Text, Text, Text, Text, Maybe Text)]) <- queryLogs
    let vmsgs = Vec.fromList $ map (view _2) rls
    return $ makePredictionTree vmsgs

-- | Make a prediction tree:
--   a map of words and the associated vector, which again contains
--   a map of trailing words (at any position, not just directly
--   following) and their frequencies.
makePredictionTree :: Vector Text -> Map Idiom $ Vector $ Map Idiom Int
makePredictionTree msgs = appendPredictionTree msgs Map.empty

appendPredictionTree :: Vector Text
                     -> Map Idiom $ Vector $ Map Idiom Int
                     -> Map Idiom $ Vector $ Map Idiom Int
appendPredictionTree msgs pt = Vec.foldr wordsToMap pt (wordGroups msgs)

wordGroups :: Vector Text -> Vector $ Vector Idiom
wordGroups = Vec.map (Vec.map mkIdiom) . join . Vec.map msgWordGroups

msgWordGroups :: Text -> Vector $ Vector Text
msgWordGroups t =
    let ws = Vec.fromList $ Text.words t
        ns = Vec.iterateN (Vec.length ws) (+1) 0
    in Vec.takeWhile (/= []) $ Vec.scanl (\b _ -> Vec.drop 1 b) ws ns

wordsToMap :: m ~ (Map Idiom $ Vector $ Map Idiom Int)
           => Vector Idiom -> m -> m
wordsToMap ws m = maybe m id $ Map.alter (addWordsCounts $ vtailSafe ws) <$> Vec.headM ws <*> Just m
  where
    vtailSafe v = if Vec.null v then v else Vec.tail v

-- FIXME you need to pad BOTH zip arguments otherwise ms will be
--       truncated
-- XXX is there a better function than zipWith?
--     mapI and indexM?
increaseWordsCounts :: ms ~ Vector (Map Idiom Int)
                    => Vector Idiom -> ms -> ms
increaseWordsCounts ws ms = Vec.zipWith zipper padws padms
  where
    zipper Nothing m = m
    zipper (Just w) m = Map.alter (Just . maybe 1 (+1)) w m
    padms = ms <> Vec.replicate (Vec.length ws - Vec.length ms) Map.empty
    padws = fmap Just ws <> Vec.replicate (Vec.length ms - Vec.length ws) empty

-- XXX this is the criminal!!!
--     it's probably the fallback empty list?
--     WHY DO WORDS GET REPEATED IN INDEXES
--     Also they get overwritten, e.g. "mommy" is empty
addWordsCounts :: m ~ Vector (Map Idiom Int)
               => Vector Idiom -> Maybe m -> Maybe m
addWordsCounts ws m = Just $ maybe ($ []) (&) m $ increaseWordsCounts ws

-- }}}

