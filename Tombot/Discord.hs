
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveGeneric,
             TypeOperators, BangPatterns, FlexibleContexts,
             EmptyDataDecls, OverloadedLists, PartialTypeSignatures #-}

-- {{{ Exports

module Tombot.Discord
    ( runDiscord
    )
    where

-- }}}

-- {{{ Imports

import qualified Tombot.Hub as Tombot
import qualified Tombot.IRC as Tombot
import qualified Tombot.Parser as Tombot
import Tombot.Types (lowerFromJSON, lowerToEncoding, lowerToJSON)
import qualified Tombot.Types as Tombot
import qualified Tombot.Utils as Tombot

import Control.Concurrent (forkIO, threadDelay, myThreadId)
import Control.Concurrent.STM
import Control.Exception (try, SomeException(..))
import Control.Lens hiding ((.=), op)
import Control.Monad (forever, join, unless, void, when, forM_)
import Control.Monad.IO.Class
import Control.Monad.Trans.State

import Data.Aeson
import Data.Aeson.Types (Options(..))
import Data.Char (toLower, toUpper)
import qualified Data.CaseInsensitive as CI
import Data.IORef
import Data.List (isPrefixOf)
import qualified Data.HashMap as HM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, listToMaybe)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy.Char8 as BL

import GHC.Generics (Generic(..))
import GHC.IO.Encoding (setLocaleEncoding, utf8)

import Network.WebSockets
import Network.Wreq hiding (get)
import Network.Wreq.Types

import System.IO (stdin)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomRIO)

import Wuss (runSecureClient)

-- }}}


-- {{{ Data

type f $ a = f a


data Identify = Identify { identifyToken :: Text
                         , identifyProperties :: Map Text Text
                         , identifyCompress :: Bool
                         , identifyLarge_threshold :: Int
                         , identifyShard :: [Int]
                         }
                         deriving (Generic, Show)

instance ToJSON Identify where
    toEncoding = lowerToEncoding 8
    toJSON = lowerToJSON 8

data Dispatch a = Dispatch { dispatchOP :: Int
                           , dispatchD :: a
                           , dispatchS :: Maybe Int
                           , dispatchT :: Maybe Text
                           }
                           deriving (Generic, Show)

instance ToJSON a => ToJSON (Dispatch a) where
    toJSON (Dispatch op d ms mt) = object list
      where
        consMay attr = maybe id ((:) . (attr .=))
        conss = consMay "s" ms . consMay "t" mt
        list = conss ["op" .= op, ("d", toJSON d) ]

--instance ToJSON a => ToJSON (Dispatch a) where
--    toEncoding = genericToEncoding $ defaultOptions { omitNothingFields = True }

instance FromJSON a => FromJSON (Dispatch a) where
    parseJSON = lowerFromJSON 8

data MessageCreate = MessageCreate { messagecTTS :: Bool
                                   , messagecTimestamp :: Text
                                   , messagecPinned :: Bool
                                   , messagecNonce :: Maybe Text
                                   , messagecMentions :: [Text]
                                   , messagecMention_roles :: [Text]
                                   , messagecMention_everyone :: Bool
                                   , messagecId :: Text
                                   , messagecEmbeds :: [Text]
                                   , messagecEdited_timestamp :: Maybe Text
                                   , messagecContent :: Text
                                   , messagecChannel_id :: String
                                   , messagecAuthor :: Map Text Text
                                   , messagecAttachments :: [Text]
                                   }
                                   deriving (Generic, Show)

instance ToJSON MessageCreate where
    toEncoding = lowerToEncoding 8
instance FromJSON MessageCreate where
    parseJSON = lowerFromJSON 8

data User = User { userVerified :: Maybe Bool
                 , userUsername :: Text
                 , userMfa_enabled :: Maybe Bool
                 , userId :: Text
                 , userEmail :: Maybe Text
                 , userDiscriminator :: Text
                 , userBot :: Maybe Bool
                 , userAvatar :: Maybe Text
                 }
                 deriving (Generic, Show)

instance ToJSON User where
    toEncoding = lowerToEncoding 4
instance FromJSON User where
    parseJSON = lowerFromJSON 4

data Member = Member { memberNick :: Maybe Text
                     , memberRoles :: [Text]
                     , memberDeaf :: Bool
                     , memberMute :: Bool
                     , memberUser :: User
                     , memberJoined_at :: Text
                     }
                     deriving (Generic, Show)

instance ToJSON Member where
    toEncoding = lowerToEncoding 6
instance FromJSON Member where
    parseJSON = lowerFromJSON 6

data Guild = Guild { guildUnavailable :: Bool
                   , guildId :: Text
                   }
                   deriving (Generic, Show)

instance ToJSON Guild where
    toEncoding = lowerToEncoding 5
instance FromJSON Guild where
    parseJSON = lowerFromJSON 5

data Hello = Hello { heartbeat_interval :: Int, _trace :: [Text] }
    deriving (Generic, Show)

instance ToJSON Hello

data Ready = Ready { readyV :: Int
                   , readyUser :: User
                   , readyShard :: [Int]
                   , readySession_id :: Text
                   , readyRelationships :: Value
                   , readyPrivate_channels :: Value
                   , readyPresences :: Value
                   , readyHeartbeat_interval :: Int
                   , readyGuilds :: [Guild]
                   , ready_trace :: [Text]
                   }
                   deriving (Generic, Show)

instance ToJSON Ready where
    toEncoding = lowerToEncoding 5
instance FromJSON Ready where
    parseJSON = lowerFromJSON 5

data GuildCreate = GuildCreate { guildcVoice_states :: Value
                               , guildcVerification_level :: Int
                               , guildcUnavailable :: Bool
                               , guildcRoles :: Value
                               , guildcRegion :: Text
                               , guildcPresences :: Value
                               , guildcOwner_id :: Text
                               , guildcName :: Text
                               , guildcMfa_level :: Int
                               , guildcMembers :: [Member]
                               , guildcMember_count :: Int
                               , guildcLarge :: Bool
                               , guildcJoined_at :: Text
                               , guildcId :: Text
                               , guildcIcon :: Text
                               , guildcFeatures :: Value
                               , guildcEmojis :: Value
                               , guildcDefault_message_notifications :: Int
                               , guildcChannels :: Value
                               , guildcAFK_timeout :: Int
                               , guildcAFK_channel_id :: Maybe Text
                               }
                               deriving (Generic, Show)

instance ToJSON GuildCreate where
    toEncoding = lowerToEncoding 6
instance FromJSON GuildCreate where
    parseJSON = lowerFromJSON 6

data TypingStart = TypingStart { typingsUser_id :: Text
                               , typingsTimestamp :: Double
                               , typingsChannel_id :: Text
                               }
                               deriving (Generic, Show)

instance ToJSON TypingStart where
    toEncoding = lowerToEncoding 7
instance FromJSON TypingStart where
    parseJSON = lowerFromJSON 7

data PresenceUpdate = PresenceUpdate { presUser :: Map Text Text
                                     , presStatus :: Text
                                     , presRoles :: [Text]
                                     , presNick :: Text
                                     , presGuild_id :: Text
                                     , presGame :: Text
                                     }
                                     deriving (Generic, Show)

instance ToJSON PresenceUpdate where
    toEncoding = lowerToEncoding 4
instance FromJSON PresenceUpdate where
    parseJSON = lowerFromJSON 4


-- }}}


appId = "181425182257315840"
appSecret = "P0sOHDhBgcDnRv6uy9PQ8h7nEmWi_d0K"

botId = "181494632721547266"
botToken = "MTgxNDk0NjMyNzIxNTQ3MjY2.ChpljA.WmukAGg2GV9Q8csv5rbgAARtZ5w"

apiURL :: String
apiURL = "https://discordapp.com/api"
gatewayURL = apiURL ++ "/gateway"
messageURL channelId = "/channels/" ++ channelId ++ "/messages"

wsURL = "gateway.discord.gg"

userAgent = "DiscordBot (https://github.com/Shou, v1.0)"

opts = defaults & header "User-Agent" .~ [userAgent]
                & header "Authorization" .~ [fromString botToken]
                & header "Content-Type" .~ ["application/json"]

props = M.fromList [ ("$os", "linux")
                   , ("$browser", "Tombot")
                   , ("$device", "Tombot")
                   , ("$referrer", "")
                   , ("$referring_domain", "")
                   ]


stateSeq = unsafePerformIO $ newTMVarIO 0

stateConfigt = unsafePerformIO $ newEmptyTMVarIO

recvTChan = unsafePerformIO $ newTChanIO

stateUsers = unsafePerformIO $ newTMVarIO []


trySome :: IO a -> IO $ Either SomeException a
trySome = try

sendJSON obj conn = do
    let json = encode obj
    BL.putStrLn json
    sendTextData conn json

sendHTTP path obj = do
    r <- postWith opts (apiURL ++ path) obj
    let responseText = r ^. responseBody
    print responseText

identify :: ToJSON a => a -> Connection -> IO ()
identify obj = sendJSON $ Dispatch 2 obj Nothing Nothing

onReady :: Connection -> Dispatch Ready -> IO ()
onReady conn dsptch@(Dispatch op d s t) = do
    print $ dsptch { dispatchD = () }
    tid <- forkIO $ forever $ do
        let ms = readyHeartbeat_interval d * 1000
        putStr $ show ms
        threadDelay ms
        seq <- atomically $ readTMVar stateSeq
        let obj = Dispatch 1 seq Nothing Nothing
        sendJSON obj conn
    print tid

onGuildCreate :: Connection -> Dispatch GuildCreate -> IO ()
onGuildCreate conn dsptch@(Dispatch op d s t) = do
    print $ dsptch { dispatchD = () }
    let members = guildcMembers d
        users = M.unions $ flip map members $ \mem ->
            let !user = memberUser mem
                !nick = CI.mk $ userUsername user
            in M.singleton nick $
                   Tombot.User { Tombot.userNick = nick
                               , Tombot.userName = userUsername user
                               , Tombot.userId = userId user
                               , Tombot.userAvatar = userAvatar user
                               , Tombot.userHost = ""
                               , Tombot.userStat = Tombot.Online
                               , Tombot.userChans = mempty
                               }
    atomically $ swapTMVar stateUsers users
    print $ flip map members $ userUsername . memberUser

onMessage :: Connection -> Dispatch MessageCreate -> IO ()
onMessage conn dsptch@(Dispatch op d s t) = do
    print $ messagecAuthor d
    print $ messagecContent d
    atomically $ swapTMVar stateSeq $ maybe 0 id s
    atomically $ writeTChan recvTChan $ messagecContent d

    users <- atomically $ readTMVar stateUsers
    configt <- atomically $ readTMVar stateConfigt
    tid <- myThreadId
    let chan = fromString $ messagecChannel_id d
        stChan = Tombot.StChannel { Tombot.stChanName = chan
                                  , Tombot.stChanTopic = ""
                                  , Tombot.stChanJoin = True
                                  , Tombot.stChanAutoJoin = True
                                  , Tombot.stChanMode = ""
                                  , Tombot.stChanPrefix = ":"
                                  , Tombot.stChanFuncs = Tombot.Blacklist []
                                  }
        chans = [ (chan, stChan) ]
        nick = maybe "idiot" id . M.lookup "username" $ messagecAuthor d
        user = Tombot.User (CI.mk nick) "" "" Nothing "" Tombot.Online (M.singleton chan "")
        users' = flip M.map users $ \user ->
                user { Tombot.userChans = M.singleton chan "" }
        server = Tombot.StServer { Tombot.stServHost = "discordapp.com"
                                 , Tombot.stServPort = 443
                                 , Tombot.stServChans = chans
                                 , Tombot.stServBotNicks = ["Tombot"]
                                 , Tombot.stServBotName = ""
                                 , Tombot.stServNickServId = Nothing
                                 , Tombot.stServStat = Tombot.Connected
                                 , Tombot.stServUsers = users'
                                 , Tombot.stServThreads = []
                                 }
        current = Tombot.Current { Tombot.currUser = user
                                 , Tombot.currMode = ""
                                 , Tombot.currServ = server
                                 , Tombot.currDest = Right stChan
                                 , Tombot.currConfigTMVar = configt
                                 , Tombot.currHandle = stdin
                                 , Tombot.currThreadId = tid
                                 }
        privmsg = Tombot.Privmsg (CI.mk nick) "" "" chan $ messagecContent d

    s <- newTMVarIO current
    fmap fst . flip runStateT s $ do
        Tombot.printTell send privmsg
        void . Tombot.forkMi $ Tombot.onMatch send privmsg
        void . Tombot.forkMi $ Tombot.runLang send privmsg
        Tombot.logPriv privmsg

  where
    send chan msg = when (not $ T.null msg) $ do
        let msgObj = M.singleton "content" msg :: Map Text Text
        liftIO $ sendHTTP (messageURL $ T.unpack chan) (encode msgObj)

onPresUpdate :: Connection -> Dispatch PresenceUpdate -> IO ()
onPresUpdate conn dsptch = do
    print dsptch

onTypingStart :: Connection -> Dispatch TypingStart -> IO ()
onTypingStart conn dsptch = do
    print dsptch

websockIdentify conn = do
    identify (Identify (fromString botToken) props False 50 [0, 1]) conn

    websockLoop conn

websockLoop conn = do
    emsg <- trySome $ receive conn

    flip (either onClose) emsg $ \msg -> do
        mdm <- case msg of
            DataMessage dm -> case dm of
                Text t -> return . Just $ fromLazyByteString t
                Binary b -> return . Just $ fromLazyByteString b
            -- TODO
            ControlMessage cm -> print cm >> return Nothing

        -- TODO SHORTCIRCUITING NOW
        let ready = join $ decode <$> mdm
        let guildCreate = join $ decode <$> mdm
        let message = join $ decode <$> mdm
        let presUpdate = join $ decode <$> mdm
        let typingStart = join $ decode <$> mdm
        maybe (return ()) (onMessage conn) message
        maybe (return ()) (onReady conn) ready
        maybe (return ()) (onGuildCreate conn) guildCreate
        maybe (return ()) (onPresUpdate conn) presUpdate
        maybe (return ()) (onTypingStart conn) typingStart

        -- Preview all
        maybe (return ()) (print . BL.take 50) mdm

        websockLoop conn

onClose e = print e >> websockInit

websockInit = do
    runSecureClient wsURL 443 "/" websockIdentify


runDiscord configt = do
    setLocaleEncoding utf8
    atomically $ putTMVar stateConfigt configt
    websockInit

    --r <- getWith opts gatewayURL
    --let responseText = r ^. responseBody
    --    murl = join $ M.lookup "url" <$> (decode responseText :: Maybe (Map String String))

    --maybe (return()) websockInit murl

