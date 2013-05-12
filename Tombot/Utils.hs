
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
import Control.Monad.State

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import Data.Char
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
-- }}}


-- {{{ Attoparsec utils

-- | manyTill but return the end result as well.
manyTillKeep p end = scan ([], [])
  where
    scan (xs, mv) = fmap (xs,) end <|> (fmap (snoc xs) p >>= scan . (,mv))
    snoc xs x = xs `mappend` [x]

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
    return $ join $ readMay <$> ms

-- }}}

-- {{{ Monoid utils

mwhen :: (Monoid a, Monad m) => Bool -> m a -> m a
mwhen True m = m
mwhen False _ = return mempty

-- }}}

-- {{{ Parsing utils

-- }}}

-- {{{ StateT utils

puts f = do
    x <- get
    put $ f x

-- }}}

-- {{{ Config utils

-- | Modify the server's userlist.
modUserlist f = do
    s <- gets $ keepServ
    puts $ \k -> k { keepServ = s { servUsers = f $ servUsers s } }

whenStat :: Monoid a => (Status -> Bool) -> Mind a -> Mind a
whenStat p m = do
    stat <- gets (userStat . currUser . keepCurrent)
    mwhen (p stat) m

mapChans f user = user { userChans = f $ userChans user }

mapStat f user = user { userStat = f $ userStat user }

-- | Change the topic of a channel.
putTopic :: Text -> Text -> Mind ()
putTopic chan t = do
    server <- gets $ keepServ
    let mc = M.lookup chan $ servChans server
        mc' = (\c -> c { chanTopic = t }) <$> mc
        ec = note ("No channel: " <> chan) mc'
        e = flip fmap ec $ \channel -> do
            let cs = M.insert chan channel $ servChans server
            puts $ \k -> k { keepServ = server { servChans = cs } }
    either warn id e

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
putPrivmsg d t = unless (T.null t) $ write $ "PRIVMSG " <> d <> " :" <> t

forkMi m = do
    s <- get
    liftIO . forkIO . void $ runStateT m s

-- }}}

