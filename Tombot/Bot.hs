
-- This file is part of Tombot, licensed under the GNU GPL 2 license.
-- See the file "LICENSE" for more information.
-- Copyright Shou, 2013

{-# LANGUAGE OverloadedStrings #-}

module Tombot.Bot where

-- {{{ Imports
import Tombot.Net
import Tombot.IRC
import Tombot.Parser
import Tombot.Utils
import Tombot.Types

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Error
import Control.Monad.State
import Control.Monad.Trans.Either (EitherT(..))

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Timeout (timeout)
-- }}}

-- | Initialise a Mind state.
initialise :: TMVar StConfig -> Server -> IO ()
initialise configt server = do
    let host = servHost server
        port = fromIntegral $ servPort server
        nick = T.pack $ servBotNicks server !! 0
        name = T.pack $ servBotName server
    h <- connecter host port nick name
    tid <- myThreadId
    let stServer = toStServ server
        current = Current { currUser = defUser
                          , currMode = ""
                          , currServ = stServer
                          , currDest = Left defUser
                          , currConfigTMVar = configt
                          , currHandle = h
                          , currThreadId = tid
                          }
    currt <- newTMVarIO current
    atomically $ flip mapTMVar configt $ \c ->
        c { stConfServs = let servs = stConfServs c
                          in M.insert host currt servs
          }
    void $ runStateT listen currt

-- TODO fix TMVar lock
-- | Read the IRC handle.
listen :: Mind ()
listen = forever $ do
    st <- get
    mh <- liftIO $ timeout (10^6) $ fst <$> runStateT (currHandle <$> see) st
    unless (isJust mh) $ erro "There's a TMVar lock somewhere."
    let h = (\(Just h) -> h) mh
    let tryGetLine :: Mind (Maybe Text)
        tryGetLine = liftIO $ do
            fmap join $ timeout (240*10^6) $ fmap hush $ try $ T.hGetLine h
    mline <- tryGetLine
    maybe reconnect respond mline

-- | please
respond :: Text -> Mind ()
respond line = do
    server <- sees currServ
    let eirc = note line $ A.maybeResult $ A.parse ircparser line `A.feed` ""
    flip (either warn) eirc $ \irc -> void . runEitherT $ do
        liftIO $ print irc
        onNick irc $ \_ -> do
            nickUserlist irc
        onMode irc $ \_ -> do
            adaptMode irc
            changeMode irc
        onQuit irc $ \_ -> do
            quitUserlist irc
        onJoin irc $ \_ -> do
            adaptJoin irc
            addJoin irc
            remind irc
        onPart irc $ \_ -> do
            adaptPart irc
        onTopic irc $ \(Topic nick name host c t) -> do
            void $ modChanTopic c $ const t
        onPing irc $ \(Ping t) -> write $ "PONG :" <> t
        onPrivmsg irc $ \_ -> do
            adaptPriv irc
            ctcpVersion irc
            printTell irc
            void . forkMi $ onMatch irc
            void . forkMi $ runLang irc
            logPriv irc
        onInvite irc $ \_ -> do
            adaptInv irc
            joinInv irc
        onKick irc $ \_ -> do
            adaptKick irc
            botKick irc
        onNumeric "311" irc $ \_ -> do
            whoisNum irc
        onNumeric "324" irc $ \_ -> do
            modeNum irc
        onNumeric "332" irc $ \_ -> do
            topicNum irc
        onNumeric "353" irc $ \_ -> do
            adaptNum irc
            userlistNum irc
        onNumeric "376" irc $ \_ -> do
            welcomeNum irc
        onNumeric "433" irc $ \_ -> do
            cycleNick irc
        onNumeric "482" irc $ \_ -> do
            privilegeNum irc

