
-- This file is part of Tombot, licensed under the GNU GPL 2 license.
-- See the file "LICENSE" for more information.
-- Copyright Shou, 2013

{-# LANGUAGE StandaloneDeriving, ScopedTypeVariables,
             TypeSynonymInstances, FlexibleInstances,
             DeriveGeneric, FlexibleContexts, TypeFamilies,
             TemplateHaskell, OverloadedStrings, TypeApplications,
             AllowAmbiguousTypes, EmptyDataDecls, TypeOperators,
             RankNTypes, FunctionalDependencies #-}

module Tombot.Types where

-- {{{ Imports

import Control.Concurrent (ThreadId)
import Control.Concurrent.STM
import Control.Lens (makeLenses)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Type.Operator

import Combinator.Booly ( Falsifier(..), Andlike(..), Orlike(..)
                        , Xorlike(..)
                        )

import Data.Aeson
import Data.Aeson.Types
import Data.Bifunctor
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Char (toLower)
import Data.Default
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Vector (Vector)

import qualified Database.SQLite.Simple.ToField as SQL

import GHC.Generics

import Network (PortNumber)

import System.IO (Handle)

-- }}}

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


-- Services

data Discourse


type family ServerService s
type family ChannelService s
type family UserService s
type family BotService s
type family MessageService s
type family Destination s

type instance ServerService () = ()
type instance ChannelService () = ()
type instance UserService () = ()
type instance BotService () = ()
type instance MessageService () = ()
type instance Destination () = ()


instance FromJSON (CI Text) where
    parseJSON = fmap CI.mk . parseJSON


data Args = Args { argsVerbosity :: Int
                 , argsService :: Text
                 , argsConfigPath :: FilePath
                 }


data Message s = Message { _messageContent :: Text
                         , _messageUserId :: Text
                         , _messageDestination :: Destination s
                         , _messageService :: MessageService s
                         }


data Biallowed a b = Blacklist !a
                   | Whitelist !b
                   deriving (Read, Show, Generic)

type Allowed a = Biallowed a a

instance FromJSON a => FromJSON (Allowed a)

instance Bifunctor Biallowed where
    bimap f _ (Blacklist a) = Blacklist $ f a
    bimap _ f (Whitelist a) = Whitelist $ f a

data UserStatus = Offline
                | Banned
                | Online
                | Mod
                | Admin
                | BotOwner
                deriving (Eq, Ord, Show, Read)

type Mode = [Char]

type Nick = CI Text

-- TODO last activity/online
data User s = User { _userNick :: !Text
                   -- ^ Nickname
                   , _userName :: !Text
                   -- ^ Full name
                   , _userId :: !Text
                   -- ^ Account ID
                   , _userChannels :: Set (CI Text)
                   -- ^ Channel names
                   , _userStatus :: !UserStatus
                   -- ^ Online status
                   , _userService :: UserService s
                   -- ^ Service data
                   }

instance Default (User ()) where
    def = User "" "" "" mempty Offline ()

data Bot s = Bot { _botNick :: Text
                 , _botName :: Text
                 , _botId :: Text
                 , _botService :: BotService s
                 }


-- XXX what else should a `Channel' hold?
-- NOTE rejoin on kick with `chanJoin = True`; don't join at all if `False`.
data Channel s = Channel { _chanName :: CI Text
                         , _chanId :: Text
                         , _chanJoin :: !Bool
                         , _chanAutoJoin :: !Bool
                         , _chanTopic :: !Text
                         , _chanPrefix :: ![Char]
                         , _chanFuncs :: Map Text $ Funk s
                         , _chanService :: ChannelService s
                         }

instance Default (Channel ()) where
    def = Channel "" "" False False "" [':'] mempty ()

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

data Server s = Server { _servId :: !Text
                       , _servChannels :: Map Text (Channel s)
                       , _servStatus :: ServerStatus
                       , _servUsers :: Map Text (User s)
                       , _servBot :: Bot s
                       , _servService :: ServerService s
                       }

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
                     deriving (Show, Generic)


instance Default Config where
    def = Config 1 "" True "logs/" "Config.json"

instance FromJSON Config where
  parseJSON (Object v) =
    Config <$> v .: "verbosity"
           <*> v .: "directory"
           <*> v .: "logging"
           <*> v .: "logpath"
           <*> v .: "file"

instance ToJSON Config where
  toEncoding (Config v d l lp f) =
    pairs $ "verbosity" .= v <> "directory" .= d
                             <> "logging" .= l
                             <> "logpath" .= lp
                             <> "file" .= f

data Current s = Current { _currUser :: User s
                         , _currServer :: Server s
                         , _currDestination :: Either (User s) (Channel s)
                         , _currSender :: Message s -> Mind s ()
                         , _currConfig :: !Config
                         }

instance Show (Message s -> Mind s ()) where
    show _ = "_sender"

data StFunk = StFunk { stFunkRecs :: !Int
                     , stFunkMax :: !Int
                     }

data Funk s = Funk { funkName :: !Text
                   , funkFunc :: Text -> Mind s Text
                   , funkStat :: !UserStatus
                   }

instance Show (Funk s) where
    show (Funk n _ s) = "Funk " <> show n <> " " <> show s


type Mind s = ReaderT (TVar (Current s)) IO
type Decide s e = ExceptT e $ Mind s
type Funky s = StateT StFunk (Mind s)

type Modes = Map Text Mode
type Users s = Map Text (User s)

instance SQL.ToField Nick where
    toField = SQL.toField . CI.original


makeLenses ''User
makeLenses ''Bot
makeLenses ''Channel
makeLenses ''Server
makeLenses ''Config
makeLenses ''Current
makeLenses ''Message


