
-- This file is part of Tombot, licensed under the GNU GPL 2 license.
-- See the file "LICENSE" for more information.
-- Copyright Shou, 2013

{-# LANGUAGE OverloadedStrings #-}

module Tombot.IRC.Net where


-- {{{ Imports
import Tombot.Utils
import Tombot.Types

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Exception (SomeException)
import qualified Control.Exception as E
import qualified Control.Lens as Lens
import Control.Monad.State

import Data.Bool (bool)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Network

import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout
-- }}}


{-# NOINLINE handleVar #-}
handleVar = unsafePerformIO newEmptyTMVarIO


-- XXX is it better to move hPutStrLn elsewhere?
-- | IRC network server connecting function that waits for a successful
-- connection and returns the Handle.
connecter :: MonadIO m => String -> PortNumber -> Text -> Text -> m Handle
connecter host port nick name = liftIO $ connecter' 0
  where
    connecter' :: Int -> IO Handle
    connecter' n = E.handle (onerror n) $ do
        unless (n < 1) $ do
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
            return h

        maybe (connecter' $ inctime n) pure mh

    inctime n = min 300 $ max n 1 * 2
    onerror :: Int -> SomeException -> IO Handle
    onerror n e = erro e >> connecter' (inctime n)

-- | Mind IRC server connector.
connect :: Mind s ()
connect = do
    server <- sees _currServer

    let host = _servHost server
        port = _servPort server
        nick = _botNick $ _servBot server
        name = _botName $ _servBot server

    verb $ "Connecting to " <> host

    h <- connecter (T.unpack host) (fromInteger port) nick name

    liftIO . atomically $ do
        b <- isEmptyTMVar handleVar
        if b then putTMVar handleVar h
             else void $ swapTMVar handleVar h

    sets $ Lens.over currServer $ Lens.set servStatus Connected

-- | Reconnect to the IRC network server.
reconnect :: Mind s ()
reconnect = do
    h <- liftIO . atomically $ readTMVar handleVar
    liftIO $ hClose h
    connect

