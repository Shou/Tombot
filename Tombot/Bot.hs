
-- This file is part of Tombot, licensed under the GNU GPL 2 license.
-- See the file "LICENSE" for more information.
-- Copyright Shou, 2013

{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Tombot.Bot where

-- {{{ Imports
import Tombot.IRC.Net
import qualified Tombot.IRC.Types as IRC
import Tombot.IRC
import Tombot.Parser
import Tombot.Utils
import Tombot.Types

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Error
import Control.Monad.Reader
import Control.Monad.Except

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Atto
import qualified Data.CaseInsensitive as CI
import Data.Default
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import System.Timeout (timeout)
-- }}}

-- | Initialise a Mind state.
initialise :: TVar Config -> Server IRC.IRC -> IO ()
initialise configt server = do
    config <- atomically $ readTVar configt

    let host = Text.unpack $ _servId server
        port = fromIntegral $ IRC._servPort $ _servService server
        -- TODO FIXME XXX
        nick = "Tombot"
        name = "Tombot"

    h <- connecter host port nick name

    tid <- myThreadId

    let current = Current { _currUser = def
                          , _currServer = server
                          , _currDestination = Left def
                          , _currConfig = config
                          }

    currt <- newTVarIO current
    void $ runReaderT listen currt

-- TODO fix TMVar lock
-- | Read the IRC handle.
listen :: Mind IRC.IRC ()
listen = forever $ do
    let tryGetLine :: Mind IRC.IRC (Maybe Text)
        tryGetLine = liftIO $ do
            fmap join $ timeout (240*10^6) $ fmap hush $ try $ undefined
    mline <- tryGetLine
    maybe reconnect respond mline

-- | please
respond :: Text -> Mind IRC.IRC ()
respond line = do
    server <- sees _currServer
    let eirc = note line $ Atto.maybeResult $ Atto.parse ircparser line `Atto.feed` ""
    flip (either warn) eirc $ \irc -> void . runExceptT $ do
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
        onTopic irc $ \(IRC.Topic nick name host c t) -> do
            void $ modChanTopic c $ const t
        onPing irc $ \(IRC.Ping t) -> write "IRC" $ "PONG :" <> t
        onPrivmsg irc $ \_ -> do
            adaptPriv irc
            ctcpVersion irc
            printTell putPrivmsg irc
            void . forkMi $ onMatch putPrivmsg irc
            void . forkMi $ runLang putPrivmsg irc
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

