
-- This file is part of Tombot, licensed under the GNU GPL 2 license.
-- See the file "LICENSE" for more information.
-- Copyright Shou, 2013

{-# LANGUAGE OverloadedStrings #-}

module Tombot.Net where


-- {{{ Imports
import Tombot.Utils
import Tombot.Types

import Control.Applicative
import Control.Concurrent
import Control.Exception (SomeException)
import qualified Control.Exception as E
import Control.Monad.State

import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Network

import System.IO
import System.Timeout
-- }}}

-- TODO remove the Utils import by adding the Utils here, then we can import
--      this module everywhere

-- XXX is it better to move hPutStrLn elsewhere?
-- | IRC network server connecting function that waits for a successful
-- connection and returns the Handle.
connecter :: MonadIO m => String -> PortNumber -> Text -> Text -> m Handle
connecter host port nick name = liftIO $ connecter' 0
  where
    connecter' :: Int -> IO Handle
    connecter' n = E.handle (onerror n) $ do
        verb $ "Connection delay by " <> show n <> " seconds"
        threadDelay (10^6 * n)
        mh <- timeout (10^7) $ do
            h <- connectTo host $ PortNumber port
            te <- mkTextEncoding "UTF-8//TRANSLIT"
            hSetEncoding h te
            hSetBuffering h LineBuffering
            hSetNewlineMode h (NewlineMode CRLF CRLF)
            T.hPutStrLn h $ "NICK " <> nick
            T.hPutStrLn h $ "USER " <> name <> " 0 * :" <> name
            T.hPutStrLn h $ "CAP REQ :multi-prefix"
            return h
        maybe (connecter' $ inctime n) pure mh
    inctime n = min 300 $ max n 1 * 2
    onerror :: Int -> SomeException -> IO Handle
    onerror n e = erro e >> connecter' (inctime n)

-- | Mind IRC server connector.
connect :: Mind ()
connect = do
    server <- sees currServ
    let host = stServHost server
        port = stServPort server
        nick = stServBotNicks server !! 0
        name = stServBotName server
    verb $ "Connecting to " <> host
    h <- connecter host port nick name
    sets $ \k -> k { currHandle = h
                   , currServ = server { stServStat = Connected }
                   }

-- | Reconnect to the IRC network server.
reconnect :: Mind ()
reconnect = do
    h <- sees $ currHandle
    liftIO $ hClose h
    connect

