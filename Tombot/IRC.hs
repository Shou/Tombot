
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

-- {{{ Events

onPing :: IRC -> (IRC -> Mind ()) -> Decide () ()
onPing p@(Ping _) mind = lift (mind p) >> left ()
onPing _ _ = right ()

onNumeric :: Text -> IRC -> (IRC -> Mind ()) -> Decide () ()
onNumeric t n@(Numeric num chan text) mind | t == num = lift (mind n) >> left ()
onNumeric _ _ _ = right ()

onInvite :: IRC -> (IRC -> Mind ()) -> Decide () ()
onInvite i@(Invite _ _ _ _ _) mind = lift (mind i) >> left ()
onInvite _ _ = right ()

onPrivmsg :: IRC -> (IRC -> Mind ()) -> Decide () ()
onPrivmsg p@(Privmsg nick name host d t) mind = lift (mind p) >> left ()
onPrivmsg _ _ = right ()

onMode :: IRC -> (IRC -> Mind ()) -> Decide () ()
onMode m@(Mode nick name host chan chars text) mind = lift (mind m) >> left ()
onMode _ _ = right ()

onNick :: IRC -> (IRC -> Mind ()) -> Decide () ()
onNick j@(Nick nick name host text) mind = lift (mind j) >> left ()
onNick _ _ = right ()

onQuit :: IRC -> (IRC -> Mind ()) -> Decide () ()
onQuit q@(Quit nick name host text) mind = lift (mind q) >> left ()
onQuit _ _ = right ()

onJoin :: IRC -> (IRC -> Mind ()) -> Decide () ()
onJoin j@(Join nick name host chan) mind = lift (mind j) >> left ()
onJoin _ _ = right ()

onPart :: IRC -> (IRC -> Mind ()) -> Decide () ()
onPart p@(Part nick name host chan text) mind = lift (mind p) >> left ()
onPart _ _ = right ()

onTopic :: IRC -> (IRC -> Mind ()) -> Decide () ()
onTopic t@(Topic nick name host chan text) mind = lift (mind t) >> left ()
onTopic _ _ = right ()

-- TODO chans, nicks
onKick :: IRC -> (IRC -> Mind ()) -> Decide () ()
onKick p@(Kick nick name host chans nicks text) mind = lift (mind p) >> left ()
onKick _ _ = right ()

-- }}}

-- {{{ Join

joinUserlist :: IRC -> Mind ()
joinUserlist (Join nick name host chan) = do
    modUserlist stChanAdd
  where
    stChanAdd users =
        let mu = mapChans (M.insert chan "") <$> M.lookup nick users
            cs = M.fromList [(chan, "")]
            us = fromJust $ mu <|> Just (User nick name host UserStat cs)
        in M.insert nick us users

-- }}}

-- {{{ Kick

kickUserlist :: IRC -> Mind ()
kickUserlist (Kick nick name host chans nicks text) = do
    modUserlist stChanDel
  where
    stChanDel users =
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
    modUserlist stChanDel
  where
    stChanDel users =
        let mu = mapChans (M.delete chan) <$> M.lookup nick users
            cs = M.empty
            us = fromJust $ mu <|> Just (User nick name host UserStat cs)
        in M.insert nick us users

-- }}}

-- {{{ Privmsg

-- TODO add new Channel if "No channel"
-- TODO put rest of data from Channel in Current.
-- XXX If it's a private message and the user does not exist, add it.
--     If it's a channel message and the user does not exist, warn.
adaptSanae :: IRC -> Mind ()
adaptSanae (Privmsg nick name host d _) = do
    current <- get
    server <- gets currServ
    mc <- gets $ M.lookup d . stServChans . currServ
    users <- gets $ stServUsers . currServ
    let mu = mapChans (M.insert d "") <$> M.lookup nick users
        chans = M.fromList [(d, "")]
        user = fromJust $ mu <|> Just (User nick name host UserStat chans)
        edest = if isChan d
                then Right . fromJust $ mc <|> Just (defStChan { stChanName = d })
                else Left user
        current' = current { currDest = edest
                           , currUser = user
                           }
    modUserlist $ M.insert nick user
    put current'
  where
    isChan x | T.length x > 0 = T.head x == '#'
             | otherwise = False

-- | Respond to CTCP VERSION.
ctcpVersion :: IRC -> Mind ()
ctcpVersion irc = do
    current <- get
    let t = privText irc
        c = either userNick stChanName $ currDest current
        v = "VERSION " <> version
    mwhen (t == "\SOHVERSION\SOH") $ putPrivmsg c $ ctcp v

-- TODO only fork whe- actually, just put the Keeper data in a TMVar. ???
runLang :: IRC -> Mind ()
runLang (Privmsg nick name host d t) = do
    current <- get
    let server = currServ current
        configt = currConfigTMVar current
    funcs <- fmap (confFuncs . fst) $ liftIO $ atomically $ readTMVar configt
    let echan = note ("No channel: " <> d) $ M.lookup d $ stServChans server
        e = flip fmap echan $ \chan -> do
        let parser = botparser (stChanPrefix chan) (M.keys funcs)
            mkl = A.maybeResult . flip A.feed "" $ A.parse parser t
            me = flip fmap mkl $ \kl -> void . forkMi $ do
                t <- compile funcs kl
                putPrivmsg d t
        maybe (return ()) id me
    either warn id e

printTell :: IRC -> Mind ()
printTell (Privmsg nick _ _ dest text) = do
    serv <- gets $ stServHost . currServ
    tmvar <- gets currConfigTMVar
    dir <- liftIO . fmap (confDir . fst) . atomically $ readTMVar tmvar
    mtells <- readConfig $ dir <> "tell"
    let mchans = join $ M.lookup serv <$> mtells
        musers = join $ M.lookup dest <$> mchans
        mtexts = join $ M.lookup nick <$> musers
        msgs = maybe [] id mtexts
        (msg, msgs') = maybeTwo msgs
        users = maybe (M.singleton nick msgs) (M.insert nick msgs') musers
        chans = maybe (M.singleton dest users) (M.insert dest users) mchans
        servers = maybe (M.singleton serv chans) (M.insert serv chans) mtells
    when (isJust msg) $ do
        writeConfig (dir <> "tell") servers
        putPrivmsg dest $ nick <> ", " <> fromJust msg
  where
    maybeTwo :: [Text] -> (Maybe Text, [Text])
    maybeTwo [] = (Nothing, [])
    maybeTwo (x:[]) = (Just x, [])
    maybeTwo (x:y:z:xs) | T.length (x <> y <> z) < 420 =
        let x' = Just $ x <> " | " <> y <> " | " <> z
        in (x', xs)
    maybeTwo (x:y:xs) | T.length (x <> y) < 420 = (Just $ x <> " | " <> y, xs)
                      | otherwise = (Just x, y:xs)

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
    server <- gets currServ
    let cs = stServChans server
        mc = M.lookup chan cs
        mc' = (\c -> c { stChanTopic = text }) <$> mc
        ec = note ("No channel: " <> chan) mc'
        cs' = either (const $ cs) (flip (M.insert chan) cs) ec
    puts $ \k -> k { currServ = server { stServChans = cs' } }

-- }}}

