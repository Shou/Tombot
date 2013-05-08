
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Tombot.Utils where

-- {{{ Imports
import Tombot.Types

import Control.Applicative
import Control.Concurrent.STM
import Control.Error
import Control.Exception (SomeException)
import qualified Control.Exception as E
import Control.Monad.State

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
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

-- {{{ Monoid utils

mwhen :: (Monoid a, Monad m) => Bool -> m a -> m a
mwhen True m = m
mwhen False _ = return mempty

-- }}}

-- {{{ Parsing utils

-- }}}

-- {{{ StateT utils

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

-- TODO this is a bad category
-- {{{ Bot utils

-- XXX Do we make functions that use warn and verb, or do we use a verbosity
--     checking function, comparable to `when', except only taking one argument

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

-- }}}

