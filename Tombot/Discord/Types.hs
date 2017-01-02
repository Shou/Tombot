
{-# LANGUAGE TemplateHaskell, TypeFamilies, StandaloneDeriving,
             EmptyDataDecls, FlexibleInstances
#-}

module Tombot.Discord.Types where

import qualified Tombot.Types as Bot

import Control.Lens (makeLenses)

import Data.Default (Default(..))
import Data.Text (Text)


data Discord


type instance Bot.ServerService Discord = ()
type instance Bot.ChannelService Discord = ()
type instance Bot.UserService Discord = User
type instance Bot.BotService Discord = ()
type instance Bot.MessageService Discord = ()
-- | Channel ID
type instance Bot.Destination Discord = Text

deriving instance Show (Bot.Bot Discord)
deriving instance Show (Bot.User Discord)
deriving instance Show (Bot.Channel Discord)
deriving instance Show (Bot.Server Discord)
deriving instance Show (Bot.Current Discord)


data User = User { _userDiscriminator :: Text
                 , _userGame :: Maybe Text
                 , _userRoles :: [Text]
                 }
                 deriving (Show)

instance Default User where
    def = User mempty mempty mempty

instance Default (Bot.User Discord) where
  def = Bot.User mempty mempty mempty mempty Bot.Offline def

makeLenses ''User

