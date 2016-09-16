
-- This file is part of Tombot, licensed under the GNU GPL 2 license.
-- See the file "LICENSE" for more information.
-- Copyright Shou, 2013

{-# LANGUAGE StandaloneDeriving, ScopedTypeVariables,
             TypeSynonymInstances, FlexibleInstances,
             DeriveGeneric, FlexibleContexts,
             TemplateHaskell #-}

module Tombot.Types where

-- {{{ Imports

import Control.BoolLike ( Falsifier(..), Andlike(..), Orlike(..)
                        , Xorlike(..)
                        )
import Control.Concurrent (ThreadId)
import Control.Concurrent.STM
import Control.Lens (makeLenses)
import Control.Monad.State
import Control.Monad.Trans.Either (EitherT)

import Data.Aeson
import Data.Aeson.Types
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Char (toLower)
import Data.Default
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Typeable (Typeable)
import Data.Typeable.Internal
import Data.Vector (Vector)
import qualified Data.Vector as V

import qualified Database.SQLite.Simple.ToField as Q

import GHC.Generics

import Network (PortNumber)

import System.IO (Handle)

-- }}}

-- TODO

-- XXX


-- {{{ Helper functions
lowerFromJSON n = genericParseJSON opts
  where
    opts = defaultOptions { fieldLabelModifier = map toLower . drop n }

lowerToJSON n = genericToJSON opts
  where
    opts = defaultOptions { fieldLabelModifier = map toLower . drop n }

lowerToEncoding n = genericToEncoding opts
  where
    opts = defaultOptions { fieldLabelModifier = map toLower . drop n }
-- }}}


instance Orlike (Vector a) where
    (<|<) va vb | V.null va = vb
                | otherwise = va


data Allowed a = Blacklist !a
               | Whitelist !a
               deriving (Read, Show, Generic)

instance FromJSON a => FromJSON (Allowed a)

instance Functor Allowed where
    fmap f (Blacklist a) = Blacklist $ f a
    fmap f (Whitelist a) = Whitelist $ f a

instance Show (TMVar a) where
    show x = "TMVar _"

data UserStatus = Offline
                | Banned
                | Online
                | Mod
                | Admin
                | BotOwner
                deriving (Eq, Ord, Read, Show)

type Mode = [Char]

type Nick = CI Text

-- TODO last activity/online
data User = User { _userNick :: !Nick
                 , _userName :: !Text
                 , _userId :: !Text
                 , _userAvatar :: Maybe Text
                 , _userHost :: !Text
                 , _userStat :: !UserStatus
                 , _userChans :: Map Text Mode
                 }
                 deriving (Show)

makeLenses ''User

data Bot = Bot { _botNick :: Text
               , _botName :: Text
               , _botId :: Text
               }
               deriving (Show)

makeLenses ''Bot

-- XXX what else should a `Channel' hold?
-- NOTE rejoin on kick with `chanJoin = True`; don't join at all if `False`.
data Channel = Channel { _chanName :: !Text
                       , _chanJoin :: !Bool
                       , _chanAutoJoin :: !Bool
                       , _chanTopic :: !Text
                       , _chanPrefix :: ![Char]
                       , _chanFuncs :: Allowed [Text]
                       } deriving (Show, Generic)

makeLenses ''Channel

instance Default Channel where
    def = Channel mempty False False mempty [':'] (Whitelist [])

instance FromJSON Channel

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

data ServerStatus = Connected
                  | Connecting Int
                  -- ^ Time since connection initiated
                  | Disconnected
                  deriving (Show, Eq)

-- TODO servBotNicks
-- XXX we can generate a random string and append it to the nick, then if there
--     is a NickServId we will attempt to ghost. Basically generate a random
--     string every time the current nick is in use.
data Server = Server { _servHost :: !String
                     , _servPort :: !Int
                     , _servChans :: ![Channel]
                     , _servStatus :: ServerStatus
                     , _servUsers :: Map Text User
                     } deriving (Show)

makeLenses ''Server

-- XXX should we split currConfigTMVar up?
data Current = Current { _currUser :: !User
                       , _currMode :: !Text
                       , _currServ :: !Server
                       , _currDest :: Either User Channel
                       }

makeLenses ''Current

data Config = Config { _confVerbosity :: !Int
                     -- ^ Verbosity level.
                     -- 0: None
                     -- 1: Warnings
                     -- 2: Verbose
                     , _confDirectory :: !FilePath
                     , _confLogging :: !Bool
                     , _confLogPath :: !FilePath
                     , _confFile :: !FilePath
                     }

makeLenses ''Config

instance Default Config where
    def = Config 1 "" True "logs/" "Config.json"

data StFunk = StFunk { stFunkRecs :: !Int
                     , stFunkMax :: !Int
                     }

data Funk = Funk { funkName :: !Text
                 , funkFunc :: !Func
                 , funkStat :: !UserStatus
                 }


type Mind = StateT (TMVar Current) IO
type Decide e = EitherT e Mind
type Funky = StateT StFunk Mind

type Funcs = Map Text Funk
-- FIXME move this to the botfuncs source file; don't make it hard to find.
type Func = Text -> Mind Text

type Modes = Map Text Mode
type Users = Map Nick User

instance Q.ToField Nick where
    toField = Q.toField . CI.original

-- XXX User data?
--     wat
-- {{{ IRC
data IRC = Nick { nickNick :: !Nick
                , nickName :: !Text
                , nickHost :: !Text
                , nickText :: !Text
                }
         | Mode { modeNick :: !Nick
                , modeName :: !Text
                , modeHost :: !Text
                , modeChan :: !Text
                , modeChars :: ![Char]
                , modeText :: Maybe Text
                }
         | Quit { quitNick :: !Nick
                , quitName :: !Text
                , quitHost :: !Text
                , quitText :: !Text
                }
         | Join { joinNick :: !Nick
                , joinName :: !Text
                , joinHost :: !Text
                , joinChan :: !Text
                }
         | Part { partNick :: !Nick
                , partName :: !Text
                , partHost :: !Text
                , partChan :: !Text
                , partText :: !Text
                }
         | Topic { topicNick :: !Nick
                 , topicName :: !Text
                 , topicHost :: !Text
                 , topicChan :: !Text
                 , topicText :: !Text
                 }
         | Invite { invNick :: !Nick
                  , invName :: !Text
                  , invHost :: !Text
                  , invDest :: !Text
                  , invChan :: !Text
                  }
         | Kick { kickNick :: !Nick
                , kickName :: !Text
                , kickHost :: !Text
                , kickChans :: !Text
                , kickNicks :: !Text
                , kickText :: !Text
                }
         | Privmsg { privNick :: !Nick
                   , privName :: !Text
                   , privHost :: !Text
                   , privDest :: !Text
                   , privText :: !Text
                   }
         | Notice { noticeNick :: !Nick
                  , noticeName :: !Text
                  , noticeHost :: !Text
                  , noticeDest :: !Text
                  , noticeText :: !Text
                  }
         | Kill { killNick :: !Nick
                , killText :: !Text
                }
         | Ping { pingServer :: !Text }
         | Error { errorText :: !Text }
         | Numeric { numNumber :: !Text
                   , numArgs :: Maybe Text
                   , numText :: !Text
                   }
         | Cap { capSub :: !Text
               , capText :: !Text
               }
         deriving (Show)

-- }}}

-- {{{ Hub

data Event = EventMessage { emsgUser :: !User
                          , emsgDest :: !Text
                          , emsgTime :: !POSIXTime
                          , emsgId :: !Text
                          , emsgText :: !Text
                          }

           | EventServer { esrvName :: !Text
                         , esrvId :: !Text
                         , esrvChans :: ![Text]
                         }

           | EventStatus { estsUser :: !User
                         }

           | EventLeave { elveUser :: !User
                        }

           | EventTopic { etpcText :: !Text
                        }

-- }}}

