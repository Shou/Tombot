
-- This file is part of Tombot, licensed under the GNU GPL 2 license.
-- See the file "LICENSE" for more information.
-- Copyright Shou, 2013

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
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T

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

-- |
joinUserlist :: IRC -> Mind ()
joinUserlist (Join nick name host chan) = do
    server <- sees currServ
    dir <- stConfDir <$> readConfig
    let servhost = stServHost server
    musers <- join . fmap (M.lookup servhost) <$> readConf (dir <> "UserStats")
    let users = stServUsers server
        mu = mapChans (M.insert chan "") <$> M.lookup nick users
        chans = M.fromList [(chan, "")]
        susers = maybe mempty id musers
        stat = maybe UserStat id $ M.lookup nick susers
        mu' = flip fmap mu $ \u -> u { userStat = stat }
        user = fromJust $ mu' <|> Just (User nick name host stat chans)
    modUserlist $ M.insert nick user

-- TODO limit to one message?
-- | Remind the user with the user's message.
remind :: IRC -> Mind ()
remind (Join cnick name host chan) = do
    mrems <- readLocalStored "remind"
    let rems = maybe mempty M.toList mrems
    forM_ rems $ \(nick, msg) -> when (cnick == nick) $ do
        putPrivmsg chan $ nick <> ": " <> msg

-- |
greet :: IRC -> Mind ()
greet (Join nick name host chan) = void . forkMi $ do
    liftIO $ threadDelay (10^6*5)
    putPrivmsg chan "Hi～ :3"

-- |
adaptJoin :: IRC -> Mind ()
adaptJoin (Join nick name host chan) = do
    current <- see
    server <- sees currServ
    mc <- sees $ M.lookup chan . stServChans . currServ
    users <- sees $ stServUsers . currServ
    let mu = mapChans (M.insert chan "") <$> M.lookup nick users
        chans = M.fromList [(chan, "")]
        user = fromJust $ mu <|> Just (User nick name host UserStat chans)
        edest = if isChan chan
                then Right . fromJust $ mc <|> Just (defStChan { stChanName = chan })
                else Left user
        current' = current { currDest = edest
                           , currUser = user
                           }
    modUserlist $ M.insert nick user
    set current'

modeJoin :: IRC -> Mind ()
modeJoin (Join nick name host chan) = do
    write $ "MODE " <> chan

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

-- TODO
-- |
adaptMode :: IRC -> Mind ()
adaptMode (Mode nick name host chan _ _) = do
    return ()

-- TODO check if UserMode is less than OP then change UserStat accordingly
-- TODO move `minus' and `plus' to Utils.
-- |
changeMode :: IRC -> Mind ()
changeMode (Mode nick name host chan chars mtext) =
    plus chars $ maybe [] T.words mtext
  where
    usermodes = "vhoaq"
    ops = "oaq"
    plus [] _ = return ()
    plus ('-':xs) ys = minus xs ys
    plus (x:xs) ys
        | any (== x) usermodes && length ys > 0 = do
            e <- modUser (ys !! 0) $ \u ->
                let chans = userChans u
                    chans' = M.adjust (sort . (x :)) chan chans
                in u { userChans = chans' }
            either warn return e
            plus xs $ tail ys
        | not $ any (== x) usermodes  = do
            e <- modChan chan $ \c ->
                c { stChanMode = x : stChanMode c }
            either warn return e
            plus xs $ tailSafe ys
        | otherwise = return ()
    minus [] _ = return ()
    minus ('+':xs) ys = plus xs ys
    minus (x:xs) ys
        | any (== x) usermodes && length ys > 0 = do
            e <- modUser (ys !! 0) $ \u ->
                let chans = userChans u
                    chans' = M.adjust (filter (/= x)) chan chans
                in u { userChans = chans' }
            either warn return e
            minus xs $ tail ys
        | not $ any (== x) usermodes  = do
            e <- modChan chan $ \c ->
                c { stChanMode = filter (/= x) (stChanMode c) }
            either warn return e
            minus xs $ tailSafe ys
        | otherwise = return ()

-- }}}

-- {{{ Nick

nickUserlist :: IRC -> Mind ()
nickUserlist (Nick nick name host text) = do
    server <- sees currServ
    dir <- stConfDir <$> readConfig
    let servhost = stServHost server
    musers <- join . fmap (M.lookup servhost) <$> readConf (dir <> "UserStats")
    let users = stServUsers server
        mu = M.lookup nick users
        susers = maybe mempty id musers
        stat = maybe UserStat id $ M.lookup text susers
        mu' = flip fmap mu $ \u -> u { userNick = text, userStat = stat }
        user = fromJust $ mu' <|> Just (User text name host stat mempty)
    modUserlist $ M.insert text user . M.delete nick

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

-- |
adaptPart :: IRC -> Mind ()
adaptPart (Part nick name host chan _) = do
    current <- see
    server <- sees currServ
    mc <- sees $ M.lookup chan . stServChans . currServ
    users <- sees $ stServUsers . currServ
    let mu = mapChans (M.insert chan "") <$> M.lookup nick users
        chans = M.fromList [(chan, "")]
        user = fromJust $ mu <|> Just (User nick name host UserStat chans)
        edest = if isChan chan
                then Right . fromJust $ mc <|> Just (defStChan { stChanName = chan })
                else Left user
        current' = current { currDest = edest
                           , currUser = user
                           }
    modUserlist $ M.insert nick user
    set current'

-- }}}

-- {{{ Privmsg

-- TODO add new Channel if "No channel"
-- TODO put rest of data from Channel in Current.
-- XXX If it's a private message and the user does not exist, add it.
--     If it's a channel message and the user does not exist, warn.
adaptPriv :: IRC -> Mind ()
adaptPriv (Privmsg nick name host d _) = do
    current <- see
    server <- sees currServ
    mc <- sees $ M.lookup d . stServChans . currServ
    users <- sees $ stServUsers . currServ
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
    set current'

-- | Respond to CTCP VERSION.
ctcpVersion :: IRC -> Mind ()
ctcpVersion irc = do
    current <- see
    let t = privText irc
        c = either userNick stChanName $ currDest current
        v = "VERSION " <> version
    mwhen (t == "\SOHVERSION\SOH") $ putPrivmsg c $ ctcp v

-- TODO only fork whe- actually, just put the Keeper data in a TMVar. ???
runLang :: IRC -> Mind ()
runLang (Privmsg nick name host d t) = do
    current <- see
    let server = currServ current
        configt = currConfigTMVar current
    funcs <- fmap stConfFuncs $ liftIO $ atomically $ readTMVar configt
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
    serv <- sees $ stServHost . currServ
    tmvar <- sees currConfigTMVar
    dir <- liftIO . fmap stConfDir . atomically $ readTMVar tmvar
    mtells <- readConf $ dir <> "tell"
    let mchans = join $ M.lookup serv <$> mtells
        musers = join $ M.lookup dest <$> mchans
        mtexts = join $ M.lookup nick <$> musers
        msgs = maybe [] id mtexts
        (msg, msgs') = maybeTwo msgs
        users = maybe (M.singleton nick msgs) (M.insert nick msgs') musers
        chans = maybe (M.singleton dest users) (M.insert dest users) mchans
        servers = maybe (M.singleton serv chans) (M.insert serv chans) mtells
    when (isJust msg) $ do
        writeConf (dir <> "tell") servers
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

onMatch :: IRC -> Mind ()
onMatch irc@(Privmsg nick name host dest text) = do
    dir <- fmap stConfDir readConfig
    mons <- readLocalStored $ "respond"
    let ons = maybe mempty M.toList mons
    forM_ ons $ \(match, (ins, resp)) -> do
        let regex = mkRegexWithOpts match False ins
        emins <- try $ return $! matchRegex regex $ T.unpack text
        let e = flip fmap emins $ \mins -> do
            let m = flip fmap mins $ \ins -> do
                -- FIXME who needs indexes larger than 9 anyway?!?!
                let indexes = [ '\\' : show x | x <- [0 ..]]
                    replacer = zipWith replace indexes ins
                    resp' = foldr ($) resp replacer
                    resp'' = T.unpack $ T.replace "%nick%" nick (T.pack resp')
                runLang $ irc { privText = T.pack resp'' }
            maybe (return ()) id m
        either warn id e
  where
    replace a b c = T.unpack $ T.replace (T.pack a) (T.pack b) (T.pack c)

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
    server <- sees currServ
    let cs = stServChans server
        mc = M.lookup chan cs
        mc' = (\c -> c { stChanTopic = text }) <$> mc
        ec = note ("No channel: " <> chan) mc'
        cs' = either (const $ cs) (flip (M.insert chan) cs) ec
    sets $ \k -> k { currServ = server { stServChans = cs' } }

-- }}}

-- {{{ Invite

-- TODO check chanJoin
-- |
joinInv :: IRC -> Mind ()
joinInv (Invite nick name host dest chan) = do
    edest <- sees currDest
    let e = flip fmap edest $ \c -> do
        mwhen (stChanJoin c) $ write $ "JOIN " <> chan
    either (warn . noChan . userNick) id e
  where
    noChan = ("Not a channel, nick " <>)

-- }}}

-- {{{ Numerics

-- |
adaptNum :: IRC -> Mind ()
adaptNum (Numeric n ma t) = flip (maybe $ warn noArgs) ma $ \a -> do
    mchan <- M.lookup a <$> sees (stServChans . currServ)
    let user = Left $ User "" "" "" UserStat mempty
        dest = maybe user Right mchan
    sets $ \c -> c { currDest = dest }
  where
    noArgs = "No `numArgs' in Numeric " <> n

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
        nick = atDef "" xs 0
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
topicNum (Numeric n ma t) =
    if isJust ma
    then modChanTopic (fromJust ma) (const t) >>= verb
    else noNumeric "332"

-- TODO load `config' file with Server > Channel > Nick
-- |
userlistNum :: IRC -> Mind ()
userlistNum (Numeric n ma t) = do
    server <- sees currServ
    dir <- stConfDir <$> readConfig
    let host = stServHost server
    flip (maybe $ noNumeric "353") ma $ \chan -> do
        musers <- join . fmap (M.lookup host) <$> readConf (dir <> "UserStats")
        forM_ (T.words t) $ \modenick -> do
            let (ircmode, nick) = T.break (`notElem` "~&@%+") modenick
                mode = toMode (T.unpack ircmode)
                users = stServUsers server
                mu = mapChans (M.insert chan mode) <$> M.lookup nick users
                chans = M.fromList [(chan, mode)]
                isMod = any (`elem` "oaq") mode
                susers = maybe mempty id musers
                defstat = if isMod then OpStat else UserStat
                stat = maybe defstat id $ M.lookup nick susers
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
  where
    noArgs = "No `numArgs' in Numeric " <> n

privilegeNum :: IRC -> Mind ()
privilegeNum (Numeric n ma t) = flip (maybe $ warn noArgs) ma $ \a -> do
    write $ "PRIVMSG " <> a <> " :" <> "C-check my privileges, p-please!"
  where
    noArgs = "No `numArgs' in Numeric " <> n

-- }}}

