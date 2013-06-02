
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tombot.Types where

-- {{{ Imports

import Control.Concurrent.STM.TMVar (TMVar)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Either (EitherT)

import Data.Map (Map, empty)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Typeable.Internal

import Network (PortNumber)

import System.IO (Handle)

-- }}}

-- TODO
-- - Events

-- XXX


-- TODO test this
instance (Typeable s, Typeable1 m) => Typeable1 (StateT s m) where
    typeOf1 _ = mkTyCon3 "mtl" "Control.Monad.State.Lazy" "StateT" `mkTyConApp` [typeOf (undefined :: s), typeOf1 (undefined :: m a)]

instance Functor Allowed where
    fmap f (Blacklist a) = Blacklist $ f a
    fmap f (Whitelist a) = Whitelist $ f a

data UserStatus = OfflineStat
                | BannedStat
                | UserStat
                | OpStat
                | AdminStat
                | RootStat
                deriving (Eq, Ord, Read, Show)

data Personality = Deredere
                 -- ^ Purely soft/kind.
                 | Kuudere
                 -- ^ Cold/unassuming to soft/kind.
                 | Tsundere
                 -- ^ Cold/hostile to soft/kind.
                 | Tsunshun
                 -- ^ Cold/hostile to depressed.
                 | Yandere
                 -- ^ Soft/kind to destructive/protective/assuming.
                 deriving (Show)

data Mood = Mood { moodPers :: Personality
                 , moodGrid :: (Int, Int)
                 } deriving (Show)

type Mode = Text

-- TODO last activity/online
data User = User { userNick :: Text
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
                           , stChanMode :: Text
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

defServer = Server { servPort = 6667
                   , servChans = []
                   , servBotNicks = [ "A_Cool_Bot", "Some_Cool_Bot" ]
                   , servBotName = "CoolBot"
                   , servNickServId = ""
                   }

data StServer = StServer { stServHost :: String
                         , stServPort :: PortNumber
                         , stServChans :: Map Text StChannel
                         , stServBotNicks :: [Text]
                         , stServBotName :: Text
                         , stServNickServId :: Maybe Text
                         , stServHandle :: Handle
                         , stServStat :: ServStatus
                         , stServUsers :: Map Text User
                         } deriving (Show)

-- XXX should we split currConfigTMVar up?
data Current = Current { currUser :: User
                       , currMode :: Text
                       , currServ :: StServer
                       , currDest :: Either User StChannel
                       , currConfigTMVar :: TMVar (Config, Map String Handle)
                       } deriving (Typeable)

-- XXX we can remove `confModules' and `confDir', can't we?
--     If we just store the funcs in Config.hs, I see no reason not to.
data Config = Config { confVerbosity :: Int
                     -- ^ Verbosity level.
                     -- 0: None
                     -- 1: Warnings
                     -- 2: Verbose
                     , confLogging :: Bool
                     , confLogPath :: FilePath
                     , confPath :: FilePath
                     , confDir :: FilePath
                     , confModules :: [FilePath]
                     , confFuncs :: Funcs
                     } deriving (Typeable)

-- TODO
-- XXX what exactly should be "default"?
defConfig = Config { confVerbosity = 1
                   , confLogging = False
                   , confLogPath = ""
                   , confPath = ""
                   , confDir = ""
                   , confModules = []
                   , confFuncs = empty
                   }

data StConfig = StConfig { stConfVerb :: Int
                         , stConfLog :: Bool
                         , stConfLogPath :: FilePath
                         , stConfPath :: FilePath
                         , stConfDir :: FilePath
                         , stConfMods :: [FilePath]
                         , stConfFuncs :: Funcs
                         , stConfHandles :: Map String Handle
                         }

type Mind = StateT Current IO
type Decide e a = EitherT e Mind a

type Funcs = Map Text Func
type Func = Text -> Mind Text

type ConfigHandles = (Config, Map String Handle)

-- TODO nicer way to do `Event' data
data StEvent = StEvent (Mind (Maybe String, StEvent))

data Event = Event { evtServs :: [Server]
                   , evtMethod :: StEvent
                   }


-- XXX User data?
--     wat
-- {{{ IRC
data IRC = Nick { nickNick :: Text
                , nickName :: Text
                , nickHost :: Text
                , nickText :: Text
                }
         | Mode { modeNick :: Text
                , modeName :: Text
                , modeHost :: Text
                , modeChan :: Text
                , modeChars :: Text
                , modeText :: Maybe Text
                }
         | Quit { quitNick :: Text
                , quitName :: Text
                , quitHost :: Text
                , quitText :: Text
                }
         | Join { joinNick :: Text
                , joinName :: Text
                , joinHost :: Text
                , joinChan :: Text
                }
         | Part { partNick :: Text
                , partName :: Text
                , partHost :: Text
                , partChan :: Text
                , partText :: Text
                }
         | Topic { topicNick :: Text
                 , topicName :: Text
                 , topicHost :: Text
                 , topicChan :: Text
                 , topicText :: Text
                 }
         | Invite { invNick :: Text
                  , invName :: Text
                  , invHost :: Text
                  , invDest :: Text
                  , invChan :: Text
                  }
         | Kick { kickNick :: Text
                , kickName :: Text
                , kickHost :: Text
                , kickChans :: Text
                , kickNicks :: Text
                , kickText :: Text
                }
         | Privmsg { privNick :: Text
                   , privName :: Text
                   , privHost :: Text
                   , privDest :: Text
                   , privText :: Text
                   }
         | Notice { noticeNick :: Text
                  , noticeName :: Text
                  , noticeHost :: Text
                  , noticeDest :: Text
                  , noticeText :: Text
                  }
         | Kill { killNick :: Text
                , killText :: Text
                }
         | Ping { pingServer :: Text }
         | Error { errorText :: Text }
         | Numeric { numNumber :: Text
                   , numArgs :: Maybe Text
                   , numText :: Text
                   }
         deriving (Show)
-- }}}

