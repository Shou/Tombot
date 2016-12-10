
{-# LANGUAGE RankNTypes #-}

module Tombot.Hub where

-- {{{ Imports

import Control.Concurrent.STM
import Control.Monad.IO.Class

import Data.Text (Text)

import System.IO.Unsafe (unsafePerformIO)

-- }}}


data Connection =
    Connection { connSend :: forall m. MonadIO m => Text -> m ()
               , connRecv :: forall m. MonadIO m => m Text
               }

data Server = Server { serverName :: Text
                     , serverHost :: Text
                     , serverConn :: Connection
                     }

tmservers :: TMVar [Server]
tmservers = unsafePerformIO $ newTMVarIO []

