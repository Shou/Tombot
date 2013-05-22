
{-# LANGUAGE OverloadedStrings #-}

module Tombot.IRC where


-- {{{ Imports
import Tombot.Funcs
import Tombot.Parser
import Tombot.Types
import Tombot.Utils

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Error
import Control.Exception (SomeException)
import qualified Control.Exception as E
import Control.Monad.State

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
-- }}}

-- XXX The onX functions should return Either.
-- {{{ Events

onPing :: IRC -> (IRC -> Mind ()) -> Mind ()
onPing p@(Ping _) mind = mind p
onPing _ _ = pure ()

onNumeric :: Text -> IRC -> (IRC -> Mind ()) -> Mind ()
onNumeric t n@(Numeric num chan text) mind | t == num = mind n
onNumeric _ _ _ = pure ()

onInvite :: IRC -> (IRC -> Mind ()) -> Mind ()
onInvite i@(Invite _ _ _ _ _) mind = mind i
onInvite _ _ = pure ()

onPrivmsg :: IRC -> (IRC -> Mind ()) -> Mind ()
onPrivmsg p@(Privmsg nick name host d t) mind = mind p
onPrivmsg _ _ = pure ()

onMode :: IRC -> (IRC -> Mind ()) -> Mind ()
onMode m@(Mode nick name host chan chars text) mind = mind m
onMode _ _ = pure ()

onNick :: IRC -> (IRC -> Mind ()) -> Mind ()
onNick j@(Nick nick name host text) mind = mind j
onNick _ _ = pure ()

onQuit :: IRC -> (IRC -> Mind ()) -> Mind ()
onQuit q@(Quit nick name host text) mind = mind q
onQuit _ _ = pure ()

onJoin :: IRC -> (IRC -> Mind ()) -> Mind ()
onJoin j@(Join nick name host chan) mind = mind j
onJoin _ _ = pure ()

onPart :: IRC -> (IRC -> Mind ()) -> Mind ()
onPart p@(Part nick name host chan text) mind = mind p
onPart _ _ = pure ()

onTopic :: IRC -> (IRC -> Mind ()) -> Mind ()
onTopic t@(Topic nick name host chan text) mind = mind t
onTopic _ _ = pure ()

-- TODO chans, nicks
onKick :: IRC -> (IRC -> Mind ()) -> Mind ()
onKick p@(Kick nick name host chans nicks text) mind = mind p
onKick _ _ = pure ()

-- }}}

-- {{{ Join

joinUserlist :: IRC -> Mind ()
joinUserlist (Join nick name host chan) = do
    modUserlist chanAdd
  where
    chanAdd users =
        let mu = mapChans (M.insert chan "") <$> M.lookup nick users
            cs = M.fromList [(chan, "")]
            us = fromJust $ mu <|> Just (User nick name host UserStat cs)
        in M.insert nick us users

-- }}}

-- {{{ Kick

kickUserlist :: IRC -> Mind ()
kickUserlist (Kick nick name host chans nicks text) = do
    modUserlist chanDel
  where
    chanDel users =
        let mu = mapChans (M.delete chans) <$> M.lookup nicks users
            cs = M.empty
            us = fromJust $ mu <|> Just (User nicks name host UserStat cs)
        in M.insert nick us users

-- }}}

-- {{{ Mode



-- }}}

-- {{{ Nick

nickUserlist :: IRC -> Mind ()
nickUserlist (Nick nick name host text) = do
    modUserlist nickMod
  where
    nickMod users =
        let mu = (\u -> u { userNick = text }) <$> M.lookup nick users
            cs = M.empty
            us = fromJust $ mu <|> Just (User text name host UserStat cs)
        in M.insert text us $ M.delete nick users

-- }}}

-- {{{ Part

partUserlist :: IRC -> Mind ()
partUserlist (Part nick name host chan text) = do
    modUserlist chanDel
  where
    chanDel users =
        let mu = mapChans (M.delete chan) <$> M.lookup nick users
            cs = M.empty
            us = fromJust $ mu <|> Just (User nick name host UserStat cs)
        in M.insert nick us users

-- }}}

-- {{{ Privmsg

-- TODO put rest of data from Channel in Current.
-- XXX If it's a private message and the user does not exist, add it.
--     If it's a channel message and the user does not exist, warn.
adaptSanae :: IRC -> Mind ()
adaptSanae (Privmsg nick name host d _) = do
    current <- gets keepCurrent
    server <- gets keepServ
    mc <- gets $ M.lookup d . servChans . keepServ
    users <- gets $ servUsers . keepServ
    let mu = mapChans (M.insert d "") <$> M.lookup nick users
        chans = M.fromList [(d, "")]
        user = fromJust $ mu <|> Just (User nick name host UserStat chans)
        edest = note user mc
        current' = current { currDest = edest
                           , currUser = user
                           }
    modUserlist $ M.insert nick user
    puts $ \k -> k { keepCurrent = current' }

-- | Respond to CTCP VERSION.
ctcpVersion :: IRC -> Mind ()
ctcpVersion irc = do
    current <- gets keepCurrent
    let t = privText irc
        c = either userNick chanName $ currDest current
        v = "VERSION " <> version
    mwhen (t == "\SOHVERSION\SOH") $ putPrivmsg c $ ctcp v

-- TODO add new Channel if "No channel"
runLang :: IRC -> Mind ()
runLang (Privmsg nick name host d t) = do
    (Keeper server configt current) <- get
    funcs <- fmap (confFuncs . fst) $ liftIO $ atomically $ readTMVar configt
    let echan = note ("No channel: " <> d) $ M.lookup d $ servChans server
        e = flip fmap echan $ \chan -> do
        let parser = botparser (chanPrefix chan) (M.keys funcs)
            mkl = A.maybeResult . flip A.feed "" $ A.parse parser t
            me = flip fmap mkl $ \kl -> void . forkMi $ do
                t <- compile funcs kl
                putPrivmsg d t
        maybe (return ()) id me
    either warn id e

printTell :: IRC -> Mind ()
printTell (Privmsg nick _ _ dest text) = do
    serv <- gets $ servHost . keepServ
    tmvar <- gets keepConfigTMVar
    dir <- liftIO . fmap (confDir . fst) . atomically $ readTMVar tmvar
    mtells <- readConfig $ dir <> "tell"
    let mchans = join $ M.lookup serv <$> mtells
        musers = join $ M.lookup dest <$> mchans
        mtexts = join $ M.lookup nick <$> musers
        msgs = maybe [] id mtexts
        msg = maybeTwo msgs
        msgs' = tailSafe msgs
        users = maybe (M.singleton nick msgs) (M.insert nick msgs') musers
        chans = maybe (M.singleton dest users) (M.insert dest users) mchans
        servers = maybe (M.singleton serv chans) (M.insert serv chans) mtells
    when (isJust msg) $ do
        writeConfig (dir <> "tell") servers
        putPrivmsg dest $ nick <> ", " <> fromJust msg
  where
    maybeTwo :: [Text] -> Maybe Text
    maybeTwo [] = Nothing
    maybeTwo (x:[]) = Just x
    maybeTwo (x:y:_) | T.length (x <> y) < 420 = Just $ x <> " | " <> y

-- }}}

-- {{{ Quit

quitUserlist (Quit nick name host text) = do
    modUserlist userOff
  where
    userOff users =
        let mu = mapStat (const OfflineStat) <$> M.lookup nick users
        in maybe users (\u -> M.insert nick u users) mu

-- }}}

-- {{{ Topic

-- XXX I think this is unnecessary
addTopic :: IRC -> Mind ()
addTopic (Topic nick name host chan text) = do
    server <- gets keepServ
    let cs = servChans server
        mc = M.lookup chan cs
        mc' = (\c -> c { chanTopic = text }) <$> mc
        ec = note ("No channel: " <> chan) mc'
        cs' = either (const $ cs) (flip (M.insert chan) cs) ec
    puts $ \k -> k { keepServ = server { servChans = cs' } }

-- }}}

