
-- This file is part of Tombot, licensed under the GNU GPL 2 license.
-- See the file "LICENSE" for more information.
-- Copyright Shou, 2013

{-# LANGUAGE OverloadedStrings #-}

module Tombot.IRC where


-- {{{ Imports
import Tombot.Funcs
import Tombot.Net
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
import qualified Data.CaseInsensitive as CI
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (formatTime, getCurrentTime)

import System.Locale (defaultTimeLocale)
import System.Random (randomRIO)

import Text.Regex
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

-- | Remind the user with the user's message.
remind :: IRC -> Mind ()
remind (Join nick name host d) = unlessBanned $ do
    mrems <- readLocalStored "remind"
    let rem = maybe "" id . join $ M.lookup nick <$> mrems
    server <- sees currServ
    funcs <- stConfFuncs <$> readConfig
    void . forkMi $ do
        kl <- botparse funcs rem
        t <- if kl == mempty then return rem else compile funcs kl
        unless (T.null t) $ putPrivmsg d $ CI.original nick <> ": " <> t

-- |
greet :: IRC -> Mind ()
greet (Join nick name host chan) = void . forkMi $ do
    liftIO $ threadDelay (10^6*5)
    putPrivmsg chan "Hi～ :3"

-- |
adaptJoin :: IRC -> Mind ()
adaptJoin (Join nick name host chan) = do
    servhost <- stServHost <$> sees currServ
    dir <- stConfDir <$> readConfig
    musers <- join . fmap (M.lookup servhost) <$> readConf (dir <> "UserStats")
    let muser = join $ M.lookup nick <$> musers
        stat = maybe Online id muser
    adaptWith chan nick name host $ \u ->
        M.insert nick $ u { userStat = stat }

-- | Add a channel if it doesn't exist.
addJoin :: IRC -> Mind ()
addJoin (Join nick name host chan) = do
    chans <- stServChans <$> sees currServ
    unless (M.member chan chans) $ do
        let chans' = M.insert chan (defStChan { stChanName = chan }) chans
        sets $ \c -> c { currServ = (currServ c) { stServChans = chans' }}

-- }}}

-- {{{ Kick

-- TODO
-- XXX can `chans' and `nicks' really be several?
-- | Adapt the current from the Kick information.
adaptKick :: IRC -> Mind ()
adaptKick (Kick nick name host chan nicks text) = do
    let cinicks = CI.mk nicks
    adaptWith chan cinicks "" "" $ \u ->
        M.insert cinicks $ u { userChans = M.delete chan (userChans u) }

-- | When the bot is kicked, rejoin if `chanAutoJoin' is `True'.
botKick :: IRC -> Mind ()
botKick (Kick nick name host chans nicks text) = do
    edest <- sees currDest
    botnicks <- sees $ stServBotNicks . currServ
    let isNick = maybe False (== nicks) $ atMay botnicks 0
        e = flip fmap edest $ \c -> do
            when (stChanAutoJoin c && isNick) $ write $ "JOIN " <> stChanName c
    either (warn . noChan . origNick) id e
  where
    noChan = ("Not a channel, " <>)

-- }}}

-- {{{ Mode

-- TODO
-- |
adaptMode :: IRC -> Mind ()
adaptMode (Mode nick name host chan _ _) = do
    adaptWith chan nick name host $ M.insert nick

-- TODO move `minus' and `plus' to Utils.
-- |
changeMode :: IRC -> Mind ()
changeMode (Mode nick name host chan chars mtext) =
    mode chars $ maybe [] T.words mtext
  where
    usermodes = "vhoaq"
    mode ('+':xs) ys = plus xs ys
    mode ('-':xs) ys = minus xs ys
    mode _ _ = return ()
    plus [] _ = return ()
    plus ('-':xs) ys = minus xs ys
    plus (x:xs) ys
        | any (== x) usermodes && length ys > 0 = do
            e <- modUser (CI.mk $ ys !! 0) $ \u ->
                let chans = userChans u
                    f = maybe (Just [x]) (Just . sort . (x :))
                    chans' = M.alter f chan chans
                in u { userChans = chans' }
            either warn return e
            plus xs $ tail ys
        | not $ any (== x) usermodes = do
            e <- modChan chan $ \c ->
                c { stChanMode = x : stChanMode c }
            either warn return e
            plus xs $ tailSafe ys
        | otherwise = warn "Missing MODE argument"
    minus [] _ = return ()
    minus ('+':xs) ys = plus xs ys
    minus (x:xs) ys
        | any (== x) usermodes && length ys > 0 = do
            e <- modUser (CI.mk $ ys !! 0) $ \u ->
                let chans = userChans u
                    f = maybe (Just []) (Just . filter (/= x))
                    chans' = M.alter f chan chans
                in u { userChans = chans' }
            either warn return e
            minus xs $ tail ys
        | not $ any (== x) usermodes  = do
            e <- modChan chan $ \c ->
                c { stChanMode = filter (/= x) (stChanMode c) }
            either warn return e
            minus xs $ tailSafe ys
        | otherwise = warn "Missing MODE argument"

-- }}}

-- {{{ Nick

nickUserlist :: IRC -> Mind ()
nickUserlist (Nick nick name host text) = do
    let citext = CI.mk text
    adaptWith "" nick name host $ \u ->
        M.insert citext (u { userNick = citext }) . M.delete nick

-- }}}

-- {{{ Part

-- |
adaptPart :: IRC -> Mind ()
adaptPart (Part nick name host chan _) = do
    adaptWith chan nick name host $ \u ->
        M.insert nick $ u { userChans = M.delete chan (userChans u) }

-- }}}

-- {{{ Privmsg

-- |
adaptPriv :: IRC -> Mind ()
adaptPriv (Privmsg nick name host d _) = do
    adaptWith d nick name host $ M.insert nick

-- | Respond to CTCP VERSION.
ctcpVersion :: IRC -> Mind ()
ctcpVersion irc = do
    current <- see
    let t = privText irc
        c = either origNick stChanName $ currDest current
        v = "VERSION " <> version
    mwhen (t == "\SOHVERSION\SOH") $ putPrivmsg c $ ctcp v

-- TODO lookup user defined functions
-- |
runLang :: IRC -> Mind ()
runLang (Privmsg nick name host d t) = unlessBanned $ do
    funcs <- stConfFuncs <$> readConfig
    mlfuncs <- readLocalStored "letfuncs"
    let meval = (\f -> \g -> f . (g <>)) <$> M.lookup "eval" funcs
        allfuncs =
            if isJust meval
            then M.union funcs $ maybe mempty (M.map $ fromJust meval) mlfuncs
            else funcs
    botparse allfuncs t >>= compile allfuncs >>= putPrivmsg d

-- |
printTell :: IRC -> Mind ()
printTell (Privmsg nick _ _ dest text) = unlessBanned $ do
    serv <- sees $ stServHost . currServ
    tmvar <- sees currConfigTMVar
    dir <- liftIO . fmap stConfDir . atomically $ readTMVar tmvar
    musers <- readLocalStored "tell"
    let cinick = CI.mk nick
        mtexts = join $ M.lookup cinick <$> musers
        msgs = maybe [] id mtexts
        (msg, msgs') = pipeJoin msgs
        users = maybe (M.singleton cinick msgs) (M.insert cinick msgs') musers
    when (isJust msg) $ do
        modLocalStored "tell" $ const users
        putPrivmsg dest $ CI.original nick <> ", " <> fromJust msg

-- |
onMatch :: IRC -> Mind ()
onMatch irc@(Privmsg nick name host dest text) = unlessBanned $ do
    dir <- fmap stConfDir readConfig
    mons <- readLocalStored $ "respond"
    let ons = maybe mempty M.toList mons
    void . decide $ forM_ ons $ \(match, (ins, resp)) -> deci . decide $ do
        let regex = mkRegexWithOpts match False ins
        emins <- try $ return $! matchRegex regex $ T.unpack text
        when (isLeft emins) $ do
            verb ("onMatch: " <> show emins)
            left ()
        let mins = either (const $ Just []) id emins
        unless (isJust mins) $ left ()
        let ins = fromJust mins
            -- FIXME who needs indexes larger than 9 anyway?!?!
            indexes = [ '\\' : show x | x <- [0 .. 9]]
            replacer = zipWith replace indexes (T.unpack text : ins)
            resp' = foldr ($) resp replacer
        lift $ runLang $ irc { privText = T.pack resp' }
  where
    replace a b c = T.unpack $ T.replace (T.pack a) (T.pack b) (T.pack c)
    deci :: Mind (Either () ()) -> Decide () ()
    deci m = lift m >>= either right left

-- |
logPriv :: IRC -> Mind ()
logPriv (Privmsg nick name host dest text) = do
    logPath <- stConfLogPath <$> readConfig
    host <- stServHost . currServ <$> see
    time <- fmap T.pack . liftIO $ do
        formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" <$> getCurrentTime
    let path = logPath <> host <> " " <> T.unpack dest
        msg = T.cons '\n' $ T.intercalate "\t" [time, CI.original nick, text]
    appendFileSafe path msg

-- }}}

-- {{{ Quit

quitUserlist (Quit nick name host text) = do
    adaptWith "" nick name host $ \u -> M.insert nick $ u { userStat = Offline }

-- }}}

-- {{{ Topic

-- XXX I think this is unnecessary
addTopic :: IRC -> Mind ()
addTopic (Topic nick name host chan text) = do
    server <- sees currServ
    let cs = stServChans server
        mc = M.lookup chan cs
        mc' = (\c -> c { stChanTopic = text }) <$> mc
        ec = note ("No channel: " <> chan) mc'
        cs' = either (const $ cs) (flip (M.insert chan) cs) ec
    sets $ \k -> k { currServ = server { stServChans = cs' } }

-- }}}

-- {{{ Invite

-- | Adapt the current from the Invite information.
adaptInv :: IRC -> Mind ()
adaptInv (Invite nick name host dest chan) = do
    adaptWith chan nick name host $ M.insert nick

-- | Join on invite.
joinInv :: IRC -> Mind ()
joinInv (Invite nick name host dest chan) = unlessBanned $ do
    edest <- sees currDest
    let e = flip fmap edest $ \c -> do
        mwhen (stChanJoin c) $ write $ "JOIN " <> chan
    either (warn . noChan . origNick) id e
  where
    noChan = ("Not a channel, " <>)

-- }}}

-- {{{ Numerics

-- |
adaptNum :: IRC -> Mind ()
adaptNum (Numeric n ma t) = flip (maybe $ warn noArgs) ma $ \a -> do
    mchan <- M.lookup a <$> sees (stServChans . currServ)
    let user = Left $ User "" "" "" Online mempty
        dest = maybe user Right mchan
    sets $ \c -> c { currDest = dest }
  where
    noArgs = "No `numArgs' in Numeric " <> n

-- TODO
-- | Cycle through the available nicks, and/or attempt to GHOST the prioritized
--   nick.
cycleNick :: IRC -> Mind ()
cycleNick (Numeric n ma t) = warnDecide $ do
    server <- lift $ sees $ T.pack . stServHost . currServ
    cnicks <- lift $ sees $ stServBotNicks . currServ
    mnspw <- lift $ sees $ stServNickServId . currServ
    case cnicks of
      [] -> left $ "No nicks for " <> server
      [nick] -> lift $ do
        n <- T.pack . show <$> liftIO (randomRIO (0 :: Int, 999))
        let nicks = T.take 12 nick <> n : cnicks
        sets $ \c -> c { currServ = (currServ c) { stServBotNicks = nicks } }
      _ -> lift $ do
        let nicks = take (length cnicks) . drop 1 $ cycle cnicks
        sets $ \c -> c { currServ = (currServ c) { stServBotNicks = nicks } }
    lift reconnect
    flip (maybe $ return ()) mnspw $ \nspw -> do
        mnick <- lift . fmap headMay . sees $ stServBotNicks . currServ
        unless (isJust mnick) $ left $ "No nicks for " <> server
        let nick = fromJust mnick
        lift $ putPrivmsg "nickserv" $ "GHOST " <> nick <> " " <> nspw
        lift $ sets $ \c ->
            c {currServ = (currServ c) { stServBotNicks = cnicks } }
        lift $ write $ "NICK " <> nick

-- |
welcomeNum :: IRC -> Mind ()
welcomeNum _ = do
    server <- sees currServ
    let cs = map snd $ M.toList $ stServChans server
    forM_ cs $ \c -> when (stChanAutoJoin c) $ do
        write $ "JOIN " <> stChanName c
    let mpass = stServNickServId server
    flip (maybe $ return ()) mpass $ \pass -> do
        putPrivmsg "NickServ" $ "identify " <> pass

-- |
whoisNum :: IRC -> Mind ()
whoisNum (Numeric n ma t) = flip (maybe $ warn noArgs) ma $ \a -> do
    let xs = T.words a
        nick = CI.mk $ atDef "" xs 0
        name = atDef "" xs 1
        host = atDef "" xs 2
    e <- modUser nick $ \u -> u { userName = name
                                , userHost = host
                                }
    either warn return e
  where
    noArgs = "No `numArgs' in Numeric " <> n

-- |
topicNum :: IRC -> Mind ()
topicNum (Numeric n ma t) = void . decide $ do
    unless (isJust ma) $ lift (noNumeric "332") >> left ()
    lift $ modChanTopic (fromJust ma) (const t) >>= verb

-- TODO a way to get the name and host of users on join
-- |
userlistNum :: IRC -> Mind ()
userlistNum (Numeric n ma t) = do
    server <- sees currServ
    dir <- stConfDir <$> readConfig
    let host = stServHost server
    flip (maybe $ noNumeric "353") ma $ \chan -> do
        musers <- join . fmap (M.lookup host) <$> readConf (dir <> "UserStats")
        forM_ (T.words t) $ \modenick -> do
            let (ircmode, nick) = CI.mk <$> T.break (`notElem` "~&@%+") modenick
                mode = toMode (T.unpack ircmode)
                users = stServUsers server
                mu = mapChans (M.insert chan mode) <$> M.lookup nick users
                chans = M.fromList [(chan, mode)]
                susers = maybe mempty id musers
                stat = maybe Online id $ M.lookup nick susers
                mu' = flip fmap mu $ \u -> u { userStat = stat }
                user = fromJust $ mu' <|> Just (User nick "" "" stat chans)
            modUserlist $ M.insert nick user
            --write $ "WHOIS " <> nick

-- |
modeNum :: IRC -> Mind ()
modeNum (Numeric n ma t) = flip (maybe $ warn noArgs) ma $ \a -> do
    let (chan, mode) = dropWhile (== '+') . T.unpack <$> bisect (== ' ') a
    e <- modChan chan $ \c -> c { stChanMode = mode }
    either warn return e
    write $ "MODE " <> a
  where
    noArgs = "No `numArgs' in Numeric " <> n

privilegeNum :: IRC -> Mind ()
privilegeNum (Numeric n ma t) = flip (maybe $ warn noArgs) ma $ \a -> do
    write $ "PRIVMSG " <> a <> " :" <> "Ch-check my privileges, p-please!"
  where
    noArgs = "No `numArgs' in Numeric " <> n

-- }}}

