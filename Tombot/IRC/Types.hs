
{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, EmptyDataDecls,
             TypeFamilies, FlexibleInstances, StandaloneDeriving #-}

module Tombot.IRC.Types where


import qualified Tombot.Types as Bot

import Data.Text (Text)
import Data.Default

import Control.Lens (makeLenses)


data IRC

type instance Bot.ServerService IRC = Server
type instance Bot.ChannelService IRC = Channel
type instance Bot.UserService IRC = User
type instance Bot.BotService IRC = Bot


data Server = Server { _servPort :: Int }
  deriving (Show)

data Channel = Channel
  deriving (Show)

data User = User { _userHost :: Text
                 , _userMode :: Text
                 }
                 deriving (Show)

data Bot = Bot
  deriving (Show)

makeLenses ''User

instance Default Server where def = Server 6667
instance Default Channel where def = Channel
instance Default User where def = User mempty mempty
instance Default Bot where def = Bot

instance Default (Bot.Channel IRC) where
  def = Bot.Channel mempty mempty False False mempty [':'] mempty def

instance Default (Bot.User IRC) where
  def = Bot.User mempty mempty mempty mempty Bot.Offline def

deriving instance Show (Bot.Bot IRC)
deriving instance Show (Bot.User IRC)
deriving instance Show (Bot.Channel IRC)
deriving instance Show (Bot.Server IRC)
deriving instance Show (Bot.Current IRC)

-- {{{ IRC
data IrcAST = Nick { nickNick :: !Text
                   , nickName :: !Text
                   , nickHost :: !Text
                   , nickText :: !Text
                   }
            | Mode { modeNick :: !Text
                   , modeName :: !Text
                   , modeHost :: !Text
                   , modeChan :: !Text
                   , modeChars :: ![Char]
                   , modeText :: Maybe Text
                   }
            | Quit { quitNick :: !Text
                   , quitName :: !Text
                   , quitHost :: !Text
                   , quitText :: !Text
                   }
            | Join { joinNick :: !Text
                   , joinName :: !Text
                   , joinHost :: !Text
                   , joinChan :: !Text
                   }
            | Part { partNick :: !Text
                   , partName :: !Text
                   , partHost :: !Text
                   , partChan :: !Text
                   , partText :: !Text
                   }
            | Topic { topicNick :: !Text
                    , topicName :: !Text
                    , topicHost :: !Text
                    , topicChan :: !Text
                    , topicText :: !Text
                    }
            | Invite { invNick :: !Text
                     , invName :: !Text
                     , invHost :: !Text
                     , invDest :: !Text
                     , invChan :: !Text
                     }
            | Kick { kickNick :: !Text
                   , kickName :: !Text
                   , kickHost :: !Text
                   , kickChans :: !Text
                   , kickNicks :: !Text
                   , kickText :: !Text
                   }
            | Privmsg { privNick :: !Text
                      , privName :: !Text
                      , privHost :: !Text
                      , privDest :: !Text
                      , privText :: !Text
                      }
            | Notice { noticeNick :: !Text
                     , noticeName :: !Text
                     , noticeHost :: !Text
                     , noticeDest :: !Text
                     , noticeText :: !Text
                     }
            | Kill { killNick :: !Text
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

