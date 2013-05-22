
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

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
import qualified Data.CaseInsensitive as CI
import Data.Char
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Network.HTTP.Conduit
import Network.HTTP.Conduit.Browser
import Network.HTTP.Types.Status

import System.Timeout

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

-- }}}

-- {{{ Config utils

-- | `either' function for `Allowed' data.
allow :: (a -> b) -> (a -> b) -> Allowed a -> b
allow f _ (Blacklist a) = f a
allow _ g (Whitelist a) = g a

-- | Modify the server's userlist.
modUserlist f = do
    s <- gets $ keepServ
    puts $ \k -> k { keepServ = s { servUsers = f $ servUsers s } }

whenStat :: Monoid a => (UserStatus -> Bool) -> Mind a -> Mind a
whenStat p m = do
    stat <- gets (userStat . currUser . keepCurrent)
    mwhen (p stat) m

mapChans f user = user { userChans = f $ userChans user }

mapStat f user = user { userStat = f $ userStat user }

-- | Get the `Channel'.
getChan :: Text -> Mind (Maybe Channel)
getChan chan = do
    server <- gets keepServ
    let cs = servChans server
    return $ M.lookup chan cs

-- | Get a Channel record field.
getChanField :: Text -> (Channel -> a) -> Mind (Maybe a)
getChanField chan f = do
    channel <- getChan chan
    return $ fmap f channel

getUser :: Text -> Mind (Maybe User)
getUser nick = gets keepServ >>= return . M.lookup nick . servUsers

-- | Modify `Channel' data.
modChan :: Text -> (Channel -> Channel) -> Mind (Either Text ())
modChan chan f = do
    server <- gets keepServ
    let cs = servChans server
        mc = M.lookup chan cs
        ec = note ("No channel: " <> chan) mc
        cs' = maybe cs (flip (M.insert chan) cs . f) mc
    puts $ \k -> k { keepServ = server { servChans = cs' } }
    return $ void ec

-- | Change the topic of a channel.
modChanTopic :: Text -> (Text -> Text) -> Mind (Either Text ())
modChanTopic chan f = modChan chan $ \c -> c { chanTopic = f $ chanTopic c }

-- | Change the Funcs of a channel.
modChanFuncs :: Text -- ^ Channel
             -> (Allowed [Text] -> Allowed [Text]) -> Mind (Either Text ())
modChanFuncs chan f = modChan chan $ \c -> c { chanFuncs = f $ chanFuncs c }

-- | Change whether the bot is allowed to join a channel.
modChanJoin :: Text -> (Bool -> Bool) -> Mind (Either Text ())
modChanJoin chan f = modChan chan $ \c -> c { chanJoin = f $ chanJoin c }

-- | Change whether the bot should auto-join on reconnect/kick.
modChanAutoJoin :: Text -> (Bool -> Bool) -> Mind (Either Text ())
modChanAutoJoin chan f = modChan chan $ \c ->
    c { chanAutoJoin = f $ chanAutoJoin c }

-- | Change the function prefix characters.
modChanPrefix :: Text -> ([Char] -> [Char]) -> Mind (Either Text ())
modChanPrefix chan f = modChan chan $ \c -> c { chanPrefix = f $ chanPrefix c }

-- | Modify User data.
modUser :: Text -> (User -> User) -> Mind (Either Text ())
modUser user f = do
    server <- gets keepServ
    let us = servUsers server
        mu = M.lookup user us
        eu = note ("No user: " <> user) mu
        us' = maybe us (flip (M.insert user) us . f) mu
    puts $ \k -> k { keepServ = server { servUsers = us' } }
    return $ void eu


-- }}}

-- {{{ Exception utils

try :: IO a -> IO (Either SomeException a)
try = E.try

-- }}}

-- {{{ Funcs utils

-- TODO strip leading whitespace and newlines.
-- TODO rename function
-- | Try to read a storage file then return the Maybe result.
readConfig :: (MonadIO m, Read a) => FilePath -> m (Maybe a)
readConfig path = liftIO $ do
    ms <- fmap hush . try $ readFile path
    return $! join $ readMay <$> ms

-- Monoid or Maybe?
-- | Read a local (to the server and channel) value stored in a file in the
--   bot's path.
readLocalStored :: (MonadIO m, Read a) => FilePath -> m (Maybe a)
readLocalStored path = do
    serv <- gets $ servHost . keepServ
    edest <- gets $ currDest . keepCurrent
    tmvar <- gets keepConfigTMVar
    dir <- liftIO . fmap (confDir . fst) . atomically $ readTMVar tmvar
    mservers <- readConfig $ dir <> path
    let dest = either userName chanName edest
        mchans = join $ M.lookup serv <$> mservers
        mstoreds = join $ M.lookup dest <$> mchans
    return $ mstoreds

modLocalStored :: (MonadIO m, Read a, Read b, Show b)
               => FilePath -> (a -> b) -> m ()
modLocalStored path f = do
    serv <- gets $ servHost . keepServ
    edest <- gets $ currDest . keepCurrent
    tmvar <- gets keepConfigTMVar
    dir <- liftIO . fmap (confDir . fst) . atomically $ readTMVar tmvar
    mservers <- readConfig $ dir <> path
    let dest = either userName chanName edest
        mchans = join $ M.lookup serv <$> mtells
        mstoreds = join $ M.lookup dest <$> mchans
        storeds = maybe (const $ f mempty) f musers
        chans = maybe (M.singleton dest users) (M.insert dest users) mchans
        servers = maybe (M.singleton serv chans) (M.insert serv chans) mtells
    e <- writeConfig (dir <> "tell") servers
    either warn id e

writeConfig :: (MonadIO m, Show a) => FilePath -> a -> m (Either Text ())
writeConfig path a = liftIO $ do
    ms <- fmap louder . try $ writeFile path $ show a
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

-- }}}

-- {{{ HTTP utils

-- TODO
-- |
httpGetResponse' :: MonadIO m => String -> m (String, [(String, String)], String, String)
httpGetResponse' url = liftIO $ withManager $ \man -> do
    initReq <- parseUrl url
    let req = initReq { requestHeaders =
                            (htitle, useragent) : requestHeaders initReq
                      }
    r <- httpLbs req man
    let b = responseBody r
        s = statusCode $ responseStatus r
        v = responseVersion r
        h = responseHeaders r
    return $ ( BU.toString $ B.concat $ BL.toChunks b
             , flip map h $ \(k, v) ->
                (BU.toString $ CI.original k, BU.toString v)
             , show s
             , show v
             )
  where
    htitle = "User-Agent"
    useragent = "Mozilla/5.0 (X11; Linux x86_64; rv:10.0.2) Gecko/20100101 Firefox/10.0.2"

httpGetResponse :: MonadIO m => String -> m (String, [(String, String)], String, String)
httpGetResponse url = do
    let tryGet :: MonadIO m => m (Either (Maybe SomeException) (String, [(String, String)], String, String))
        tryGet = liftIO $ do
            mr <- timeout (10 ^ 7) (try $ httpGetResponse' url)
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
httpGetString url = liftIO $ withManager $ \man -> do
    (str, _, _, _) <- httpGetResponse url
    if length str > 10 ^ 6 * 2
        then return ""
        else return str
  where
    htitle = "User-Agent"
    useragent = "Mozilla/5.0 (X11; Linux x86_64; rv:10.0.2) Gecko/20100101 Firefox/10.0.2"

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

erro :: (MonadIO m, Show a) => a -> m ()
erro x = liftIO $ putStrLn $ "\x1b[0;31mError " <> show x <> "\x1b[0m"

-- TODO reconnect on no handle
write :: Text -> Mind ()
write t = do
    s <- gets keepServ
    let eh = note ("No handle: " <> servHost s) $ servHandle s
        e = flip fmap eh $ \h -> do
            ex <- liftIO $ E.try $ do
                T.hPutStrLn h t
                T.putStrLn $ "\x1b[0;32m" <> t <> "\x1b[0m"
            either (\e -> warn (e :: SomeException)) return ex
    either warn void e

putPrivmsg :: Text -> Text -> Mind ()
putPrivmsg d t = unless (T.null t) $ do
    write . T.take 420 $ "PRIVMSG " <> d <> " :" <> t

forkMi m = do
    s <- get
    liftIO . forkIO . void $ runStateT m s

-- }}}

-- {{{ Monoid utils

-- | When `True', run the monadic action `m', otherwise return mempty in the
--   current monad.
mwhen :: (Monoid a, Monad m) => Bool -> m a -> m a
mwhen True m = m
mwhen False _ = return mempty

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

-- {{{ Parsing utils

-- }}}

-- {{{ StateT utils

puts f = do
    x <- get
    put $ f x

-- }}}

-- {{{ Text utils

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

ctcp t = "\SOH" <> t <> "\SOH"

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

-- TODO better name; the `Hands' suffix is confusing
readConfHands :: Mind ConfigHandles
readConfHands = do
    configt <- gets keepConfigTMVar
    liftIO $ atomically $ readTMVar configt

mapConfHands :: (ConfigHandles -> ConfigHandles) -> Mind ()
mapConfHands f = do
    configt <- gets keepConfigTMVar
    liftIO $ atomically $ mapTMVar f configt

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

