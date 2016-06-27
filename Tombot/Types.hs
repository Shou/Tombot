
-- This file is part of Tombot, licensed under the GNU GPL 2 license.
-- See the file "LICENSE" for more information.
-- Copyright Shou, 2013

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Tombot.Types where

-- {{{ Imports

import Control.Concurrent (ThreadId)
import Control.Concurrent.STM
import Control.Monad.State
import Control.Monad.Trans.Either (EitherT)

import Data.CaseInsensitive (CI)
import Data.Map (Map)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Typeable.Internal

import Network (PortNumber)

import System.IO (Handle)

-- }}}

-- TODO

-- XXX



instance Functor Allowed where
    fmap f (Blacklist a) = Blacklist $ f a
    fmap f (Whitelist a) = Whitelist $ f a

instance Show (TMVar a) where
    show x = "TMVar _"

instance Show StConfig where
    show x = let verb = show $ stConfVerb x
                 dir = show $ stConfDir x
                 log = show $ stConfLog x
                 logpath = show $ stConfLogPath x
                 path = show $ stConfPath x
                 servs = show $ stConfServs x
             in concat [ "StConfig {stConfVerb = "
                       , verb
                       , ", stConfDir = "
                       , dir
                       , ", stConfLog = "
                       , log
                       , ", stConfLogPath = "
                       , logpath
                       , ", stConfPath = "
                       , path
                       , ", stConfFuncs = _"
                       , ", stConfServs = "
                       , servs
                       , "}"
                       ]

data UserStatus = Offline
                | Banned
                | Online
                | OP
                | Admin
                | Root
                deriving (Eq, Ord, Read, Show)

type Mode = [Char]

-- TODO last activity/online
data User = User { userNick :: Nick
                 , userName :: Text
                 , userHost :: Text
                 , userStat :: UserStatus
                 , userChans :: Map Text Mode
                 } deriving (Show)

-- XXX what else should a `Channel' hold?
-- NOTE rejoin on kick with `chanJoin = True`; don't join at all if `False`.
data Channel = Channel { chanName :: String
                       , chanJoin :: Bool
                       , chanAutoJoin :: Bool
                       , chanPrefix :: [Char]
                       , chanFuncs :: Allowed [String]
                       } deriving (Show)

-- | Data that's not supposed to be used in Config.
data StChannel = StChannel { stChanName :: Text
                           , stChanTopic :: Text
                           , stChanJoin :: Bool
                           , stChanAutoJoin :: Bool
                           , stChanMode :: [Char]
                           , stChanPrefix :: [Char]
                           , stChanFuncs :: Allowed [Text]
                           } deriving (Show)

data Allowed a = Blacklist a
               | Whitelist a
               deriving (Read, Show)

-- TODO move this to Utils
fromAllowed :: Allowed a -> a
fromAllowed (Blacklist a) = a
fromAllowed (Whitelist a) = a

data Assoc a = LeftA a
             | RightA a
             | CenterA a
             deriving (Eq, Show)

fromAssoc :: Assoc a -> a
fromAssoc (LeftA a) = a
fromAssoc (RightA a) = a
fromAssoc (CenterA a) = a

data ServStatus = Connected
                | Connecting Int
                -- ^ Time since connection initiated
                | Disconnected
                deriving (Show, Eq)

-- TODO servBotNicks
-- XXX we can generate a random string and append it to the nick, then if there
--     is a NickServId we will attempt to ghost. Basically generate a random
--     string every time the current nick is in use.
data Server = Server { servHost :: String
                     , servPort :: Int
                     , servChans :: [Channel]
                     , servBotNicks :: [String]
                     , servBotName :: String
                     , servNickServId :: String
                     } deriving (Show)

-- XXX consider merging Current and StServer
data StServer = StServer { stServHost :: String
                         , stServPort :: PortNumber
                         , stServChans :: Map Text StChannel
                         , stServBotNicks :: [Text]
                         , stServBotName :: Text
                         , stServNickServId :: Maybe Text
                         , stServStat :: ServStatus
                         , stServUsers :: Users
                         , stServThreads :: Map Text ThreadId
                         } deriving (Show)

-- XXX should we split currConfigTMVar up?
data Current = Current { currUser :: User
                       , currMode :: Text
                       , currServ :: StServer
                       , currDest :: Either User StChannel
                       , currConfigTMVar :: TMVar StConfig
                       , currHandle :: Handle
                       , currThreadId :: ThreadId
                       } deriving (Typeable)

data Kurrent = Kurrent { kurrUser :: User
                       , kurrMode :: Text
                       , kurrHost :: String
                       , kurrPort :: Int
                       , kurrChans :: [Channel]
                       , kurrBotNicks :: [String]
                       , kurrBotName :: String
                       , kurrNickservId :: String
                       , kurrDest :: Either User StChannel
                       , kurrHandle :: Handle
                       }

data Config = Config { confVerbosity :: Int
                     -- ^ Verbosity level.
                     -- 0: None
                     -- 1: Warnings
                     -- 2: Verbose
                     , confDir :: FilePath
                     , confLogging :: Bool
                     , confLogPath :: FilePath
                     , confPath :: FilePath
                     , confFuncs :: Funcs
                     } deriving (Typeable)

data StConfig = StConfig { stConfVerb :: Int
                         , stConfDir :: FilePath
                         , stConfLog :: Bool
                         , stConfLogPath :: FilePath
                         , stConfPath :: FilePath
                         , stConfFuncs :: Funcs
                         , stConfServs :: Map String (TMVar Current)
                         }

data StFunk = StFunk { stFunkRecs :: Int
                     , stFunkMax :: Int
                     }

data Funk = Funk { funkName :: Text
                 , funkFunc :: Func
                 , funkStat :: UserStatus
                 }


type Mind = StateT (TMVar Current) IO
type Love = StateT Kurrent IO
type Decide e = EitherT e Mind
type Funky = StateT StFunk Mind

type Funcs = Map Text Funk
type Func = Text -> Mind Text

type Modes = Map Text Mode
type Users = Map Nick User

type Nick = CI Text

-- XXX User data?
--     wat
-- {{{ IRC
data IRC = Nick { nickNick :: Nick
                , nickName :: Text
                , nickHost :: Text
                , nickText :: Text
                }
         | Mode { modeNick :: Nick
                , modeName :: Text
                , modeHost :: Text
                , modeChan :: Text
                , modeChars :: [Char]
                , modeText :: Maybe Text
                }
         | Quit { quitNick :: Nick
                , quitName :: Text
                , quitHost :: Text
                , quitText :: Text
                }
         | Join { joinNick :: Nick
                , joinName :: Text
                , joinHost :: Text
                , joinChan :: Text
                }
         | Part { partNick :: Nick
                , partName :: Text
                , partHost :: Text
                , partChan :: Text
                , partText :: Text
                }
         | Topic { topicNick :: Nick
                 , topicName :: Text
                 , topicHost :: Text
                 , topicChan :: Text
                 , topicText :: Text
                 }
         | Invite { invNick :: Nick
                  , invName :: Text
                  , invHost :: Text
                  , invDest :: Text
                  , invChan :: Text
                  }
         | Kick { kickNick :: Nick
                , kickName :: Text
                , kickHost :: Text
                , kickChans :: Text
                , kickNicks :: Text
                , kickText :: Text
                }
         | Privmsg { privNick :: Nick
                   , privName :: Text
                   , privHost :: Text
                   , privDest :: Text
                   , privText :: Text
                   }
         | Notice { noticeNick :: Nick
                  , noticeName :: Text
                  , noticeHost :: Text
                  , noticeDest :: Text
                  , noticeText :: Text
                  }
         | Kill { killNick :: Nick
                , killText :: Text
                }
         | Ping { pingServer :: Text }
         | Error { errorText :: Text }
         | Numeric { numNumber :: Text
                   , numArgs :: Maybe Text
                   , numText :: Text
                   }
         | Cap { capSub :: Text
               , capText :: Text
               }
         deriving (Show)

-- }}}

