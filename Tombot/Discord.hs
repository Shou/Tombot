
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveGeneric,
             TypeOperators, BangPatterns, FlexibleContexts,
             EmptyDataDecls, OverloadedLists, PartialTypeSignatures,
             TypeApplications
#-}

-- {{{ Exports

module Tombot.Discord
    ( runDiscord
    )
    where

-- }}}

-- {{{ Imports

import qualified Tombot.Discord.Types as Discord
import qualified Tombot.Funcs as Tombot
import qualified Tombot.IRC as IRC
import qualified Tombot.IRC.Types as IRC
import qualified Tombot.Parser as Tombot
import Tombot.Types (lowerFromJSON, lowerToEncoding, lowerToJSON)
import qualified Tombot.Types as Tombot
import qualified Tombot.Utils as Tombot

import Control.Concurrent (forkIO, threadDelay, myThreadId)
import Control.Concurrent.STM
import Control.Exception (SomeException(..))
import qualified Control.Exception as Except
import Control.Lens as Lens
import Control.Monad (forever, join, unless, void, when, forM_)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Type.Operator

import Data.Aeson as Aes
import qualified Data.Aeson.Lens as Aes
import Data.Aeson.Types (Options(..))
import Data.Char (toLower, toUpper)
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Default
import Data.IORef
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, listToMaybe, isNothing)
import Data.Monoid
import qualified Data.Set as Set
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

import System.FilePath ((</>))
import System.IO (stdin)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomRIO)

import Wuss (runSecureClient)

-- }}}


-- {{{ Data


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
        consMay attr = maybe id ((:) . (attr Aes..=))
        conss = consMay "s" ms . consMay "t" mt
        list = conss ["op" Aes..= op, ("d", toJSON d) ]

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

apiURL :: String
apiURL = "https://discordapp.com/api"
gatewayURL = apiURL ++ "/gateway"
messageURL channelId = "/channels/" ++ channelId ++ "/messages"

wsURL = "gateway.discord.gg"

userAgent = "DiscordBot (https://github.com/Shou, v1.0)"

opts botToken = defaults
              & header "User-Agent" .~ [userAgent]
              & header "Authorization" .~ ["Bot " <> fromString botToken]
              & header "Content-Type" .~ ["application/json"]

props = Map.fromList [ ("$os", "linux")
                     , ("$browser", "Tombot")
                     , ("$device", "Tombot")
                     , ("$referrer", "")
                     , ("$referring_domain", "")
                     ]


stateToken :: TMVar String
{-# NOINLINE stateToken #-}
stateToken = unsafePerformIO newEmptyTMVarIO

{-# NOINLINE stateSeq #-}
stateSeq = unsafePerformIO $ newTMVarIO 0

stateConfigt :: TMVar $ TVar Tombot.Config
{-# NOINLINE stateConfigt #-}
stateConfigt = unsafePerformIO newEmptyTMVarIO

{-# NOINLINE recvTChan #-}
recvTChan = unsafePerformIO newTChanIO

stateUsers :: TVar $ Map Text $ Map Text $ Tombot.User Discord.Discord
{-# NOINLINE stateUsers #-}
stateUsers = unsafePerformIO $ newTVarIO mempty

-- | Holding associated server id and channel name
stateChannelMetadata :: TVar $ Map Text (Text, Text)
{-# NOINLINE stateChannelMetadata #-}
stateChannelMetadata = unsafePerformIO $ newTVarIO Map.empty

getChannelMeta cid = do
    meta <- atomically $ Map.lookup cid <$> readTVar stateChannelMetadata

    return $ maybe (mempty, mempty) id meta


trySome :: IO a -> IO $ Either SomeException a
trySome = Except.try

sendJSON obj conn = do
    let json = encode obj
    BL.putStrLn json
    sendTextData conn json

sendHTTP path obj = do
    token <- atomically $ readTMVar stateToken
    r <- postWith (opts token) (apiURL ++ path) obj

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

    let serverId = guildcId d
        channels :: Aes.Array
        channels = guildcChannels d ^. Aes._Array
        channelIds = channels ^.. traverse . Aes.key "id" . Aes._String
        channelNames = channels ^.. traverse . Aes.key "name" . Aes._String
        channelTuples = zip channelIds channelNames
        inserter m (cid, cname) = Map.insert cid (serverId, cname) m
        channelMap = foldl' inserter Map.empty channelTuples

    atomically $ modifyTVar' stateChannelMetadata (channelMap <>)

    let members = guildcMembers d
        users :: Map Text $ Tombot.User Discord.Discord
        users = Map.unions $ flip map members $ \mem ->
            let !user = memberUser mem
                !mid = userId user
                !mayNick = memberNick mem
                !name = userUsername user
                !discriminator = userDiscriminator user
            in Map.singleton mid $
                   Tombot.User { Tombot._userNick = maybe name id mayNick
                               , Tombot._userName = name
                               , Tombot._userId = mid
                               , Tombot._userService = Discord.User discriminator
                               , Tombot._userStatus = Tombot.Online
                               , Tombot._userChannels = mempty
                               }

    atomically $ modifyTVar' stateUsers $ Map.insertWith Map.union serverId users
    print $ flip map members $ userUsername . memberUser

onMessage :: Connection -> Dispatch MessageCreate -> IO ()
onMessage conn dsptch@(Dispatch op d s t) = do
    print $ messagecAuthor d
    print $ messagecContent d
    atomically $ swapTMVar stateSeq $ maybe 0 id s
    atomically $ writeTChan recvTChan $ messagecContent d

    allUsers <- atomically $ readTVar stateUsers
    config <- atomically $ readTVar =<< readTMVar stateConfigt
    tid <- myThreadId

    let chanName = fromString $ messagecChannel_id d
        ciChanName = CI.mk chanName

    meta <- getChannelMeta chanName

    let serverUsers = maybe Map.empty id $ Map.lookup (view _1 meta) allUsers

    let chan :: Tombot.Channel Discord.Discord
        chan = Tombot.Channel { Tombot._chanName = CI.mk $ view _2 meta
                              , Tombot._chanId = chanName
                              , Tombot._chanTopic = ""
                              , Tombot._chanJoin = True
                              , Tombot._chanAutoJoin = True
                              , Tombot._chanPrefix = ":"
                              , Tombot._chanFuncs = Tombot.funcs
                              , Tombot._chanService = def
                              }

        chans = [ (chanName, chan) ]
        -- XXX idiot???
        nick = maybe "idiot" id . Map.lookup "username" $ messagecAuthor d
        uid = maybe "" id . Map.lookup "id" $ messagecAuthor d
        user = Tombot.User nick "" uid (Set.singleton ciChanName) Tombot.Online def
        users' = flip Map.map serverUsers
               $ Lens.over Tombot.userChannels $ Set.insert ciChanName

        bot = Tombot.Bot { Tombot._botNick = "Tombot"
                         , Tombot._botName = "Tombot"
                         , Tombot._botId = ""
                         , Tombot._botService = def
                         }

        server = Tombot.Server { Tombot._servId = view _1 meta
                               , Tombot._servChannels = chans
                               , Tombot._servBot = bot
                               , Tombot._servStatus = Tombot.Connected
                               , Tombot._servUsers = users'
                               , Tombot._servService = def
                               }

        current = Tombot.Current { Tombot._currUser = user
                                 , Tombot._currServer = server
                                 , Tombot._currDestination = Right chan
                                 , Tombot._currConfig = config
                                 }

        privmsg = IRC.Privmsg nick "" "" chanName $ messagecContent d

    s <- newTVarIO current

    flip runReaderT s $ do
        IRC.printTell send privmsg
        void . Tombot.forkMi $ IRC.onMatch send privmsg
        void . Tombot.forkMi $ IRC.runLang send privmsg
        IRC.logPriv privmsg
  where
    send chan msg = when (not $ T.null msg) $ do
        let msgObj = Map.singleton "content" msg :: Map Text Text
        liftIO $ sendHTTP (messageURL $ T.unpack chan) (encode msgObj)

onPresUpdate :: Connection -> Dispatch PresenceUpdate -> IO ()
onPresUpdate conn dsptch = do
    print dsptch

onTypingStart :: Connection -> Dispatch TypingStart -> IO ()
onTypingStart conn dsptch = do
    print dsptch

websockIdentify conn = do
    token <- atomically $ readTMVar stateToken

    print token

    identify (Identify (fromString token) props False 50 [0, 1]) conn

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

        let p = and @[] [ isNothing ready, isNothing guildCreate, isNothing message
                        , isNothing presUpdate, isNothing typingStart
                        ]

        -- print when none match
        when p $ maybe (return ()) print mdm

        websockLoop conn

onClose e = print e >> websockInit

websockInit = do
    runSecureClient wsURL 443 "/" websockIdentify


runDiscord :: TVar Tombot.Config -> IO ()
runDiscord configt = do
    setLocaleEncoding utf8

    config <- atomically $ do
        putTMVar stateConfigt configt
        readTVar configt

    let apiPath = Tombot._confDirectory config </> "api"

    apiKeys <- Tombot.readConfig @_ @(Map Text String) apiPath

    let mayToken = join $ Map.lookup "discord-token" <$> apiKeys

    case mayToken of
      Just token -> do
        atomically $ putTMVar stateToken token
        -- XXX looks dangerous
        forever $ Except.catch @SomeException websockInit print

      Nothing -> putStrLn $ "No Discord token in " <> apiPath


    --r <- getWith opts gatewayURL
    --let responseText = r ^. responseBody
    --    murl = join $ Map.lookup "url" <$> (decode responseText :: Maybe (Map String String))

    --maybe (return()) websockInit murl

