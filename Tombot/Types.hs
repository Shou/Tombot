
{-# LANGUAGE DeriveDataTypeable #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Tombot.Types where

-- {{{ Imports

import Control.Concurrent.STM.TMVar (TMVar)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Either (EitherT)

import Data.Map (Map)
import Data.Text (Text)
import Data.Typeable (Typeable)

import System.IO (Handle)

-- }}}

-- TODO
-- - Events

-- XXX


instance Show (TMVar a) where
    show _ = "TMVar _"


data Status = RootStat
            | AdminStat
            | OpStat
            | UserStat
            | BannedStat
            | OfflineStat
            deriving (Eq, Ord, Show)

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

data User = User { userNick :: Text
                 , userName :: Text
                 , userHost :: Text
                 , userStat :: Status
                 , userChans :: Map Text Mode
                 } deriving (Show)

-- XXX what else should a `Channel' hold?
-- NOTE rejoin on kick with `chanJoin = True`; don't join at all if `False`.
data Channel = Channel { chanName :: Text
                       , chanTopic :: Text
                       , chanJoin :: Bool
                       , chanAutoJoin :: Bool
                       , chanMode :: Text
                       , chanPrefix :: [Char]
                       , chanFuncs :: Allowed [Text]
                       } deriving (Show)

data Allowed a = Blacklist a
               | Whitelist a
               deriving (Show)

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
                     , servChans :: Map Text Channel
                     , servBotNick :: Text
                     , servBotName :: Text
                     , servNickServId :: Text
                     , servHandle :: Maybe Handle
                     , servTMVar :: TMVar (Map String Handle)
                     , servStat :: ServStatus
                     , servUsers :: Map Text User
                     } deriving (Show, Typeable)

-- For the sake of convenience
data Current = Current { currUser :: User
                       , currMode :: Text
                       , currServ :: String
                       , currDest :: Either User Channel
                       } deriving (Show, Typeable)

-- XXX How to do config paths?
--      - A general function that simplifies the process of checking for
--        existence, permissions, etc?
--      - Data that associates a function with a path?
data Config = Config { confVerbosity :: Int
                     , confLogging :: Bool
                     , confLogPath :: FilePath
                     , confPath :: FilePath
                     , confDir :: FilePath
                     , confModules :: [FilePath]
                     , confFuncs :: Map Text Func
                     } deriving (Typeable)

-- TODO better name for this?
data Keeper = Keeper { keepServ :: Server
                     , keepConfigTMVar :: TMVar (Config, Map String Handle)
                     , keepCurrent :: Current
                     }

-- XXX Perhaps there is a way to add the modifications made to Current to
--     Config in a better way?
type Mind = StateT Keeper IO
type Decide e a = EitherT e IO a

{-
-- TODO ugh pls ask #haskell
-- we can at least work around the issue by making the `Funcs' into
-- `Keeper -> Text -> IO Text'.
deriving instance Typeable (StateT Text IO Text)
-}

type ConfigHandles = (Config, Map String Handle)

type Funcs = Map Text Func
type Func = Text -> Mind Text

-- TODO nicer way to do `Event' data
data MEvent = MEvent (Mind (Maybe String, MEvent))

data Event = Event { evtServs :: [Server]
                   , evtMethod :: MEvent
                   }


-- XXX User data?
-- {{{ IRC
data IRC = Nick { nickNick :: Text
                , nickName :: Text
                , nickHost :: Text
                , nickText :: Text
                }
         | Mode { modeNick :: Text
                , modeName :: Text
                , modeHost :: Text
                , modeChannel :: Text
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

