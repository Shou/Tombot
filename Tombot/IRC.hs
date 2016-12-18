
-- This file is part of Tombot, licensed under the GNU GPL 2 license.
-- See the file "LICENSE" for more information.
-- Copyreturn Shou, 2013

{-# LANGUAGE OverloadedStrings, ScopedTypeVariables,
             PartialTypeSignatures, TypeApplications, FlexibleContexts
#-}

module Tombot.IRC where


-- {{{ Imports
import Tombot.Funcs
import Tombot.IRC.Types (IRC)
import Tombot.IRC.Types as IRC
import qualified Tombot.IRC.Net as IRC
import Tombot.Parser
import Tombot.Types as Bot
import Tombot.Utils as Tom

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Error
import Control.Exception (SomeException)
import qualified Control.Exception as E
import Control.Lens as Lens
import Control.Monad.State
import Control.Monad.Except

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import qualified Data.CaseInsensitive as CI
import Data.Default
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (formatTime, getCurrentTime, defaultTimeLocale)

import Database.SQLite.Simple.Types (Null(..))

import System.Random (randomRIO)

import Text.Regex
-- }}}


-- {{{ Events

onPing :: IrcAST -> (IrcAST -> Mind IRC ()) -> Decide IRC () ()
onPing p@(Ping _) mind = lift (mind p) >> throwError ()
onPing _ _ = return ()

onNumeric :: Text -> IrcAST -> (IrcAST -> Mind IRC ()) -> Decide IRC () ()
onNumeric t n@(Numeric num chan text) mind | t == num = lift (mind n) >> throwError ()
onNumeric _ _ _ = return ()

onInvite :: IrcAST -> (IrcAST -> Mind IRC ()) -> Decide IRC () ()
onInvite i@(Invite _ _ _ _ _) mind = lift (mind i) >> throwError ()
onInvite _ _ = return ()

onPrivmsg :: IrcAST -> (IrcAST -> Mind IRC ()) -> Decide IRC () ()
onPrivmsg p@(Privmsg nick name host d t) mind = lift (mind p) >> throwError ()
onPrivmsg _ _ = return ()

onMode :: IrcAST -> (IrcAST -> Mind IRC ()) -> Decide IRC () ()
onMode m@(Mode nick name host chan chars text) mind = lift (mind m) >> throwError ()
onMode _ _ = return ()

onNick :: IrcAST -> (IrcAST -> Mind IRC ()) -> Decide IRC () ()
onNick j@(Nick nick name host text) mind = lift (mind j) >> throwError ()
onNick _ _ = return ()

onQuit :: IrcAST -> (IrcAST -> Mind IRC ()) -> Decide IRC () ()
onQuit q@(Quit nick name host text) mind = lift (mind q) >> throwError ()
onQuit _ _ = return ()

onJoin :: IrcAST -> (IrcAST -> Mind IRC ()) -> Decide IRC () ()
onJoin j@(Join nick name host chan) mind = lift (mind j) >> throwError ()
onJoin _ _ = return ()

onPart :: IrcAST -> (IrcAST -> Mind IRC ()) -> Decide IRC () ()
onPart p@(Part nick name host chan text) mind = lift (mind p) >> throwError ()
onPart _ _ = return ()

onTopic :: IrcAST -> (IrcAST -> Mind IRC ()) -> Decide IRC () ()
onTopic t@(Topic nick name host chan text) mind = lift (mind t) >> throwError ()
onTopic _ _ = return ()

-- TODO chans, nicks
onKick :: IrcAST -> (IrcAST -> Mind IRC ()) -> Decide IRC () ()
onKick p@(Kick nick name host chans nicks text) mind = lift (mind p) >> throwError ()
onKick _ _ = return ()

-- }}}

-- {{{ Join

-- | Remind the user with the user's message.
remind :: IrcAST -> Mind IRC ()
remind (Join nick name host d) = unlessBanned $ do
    mrems <- readLocalStored "remind"
    let rem = maybe "" id . join $ Map.lookup nick <$> mrems
    server <- sees _currServer
    fs <- Map.union funcs <$> serverfuncs funcs
    void . forkMi $ do
        kl <- botparse fs rem
        t <- if kl == mempty then return rem else compile fs kl
        unless (T.null t) $ putPrivmsg d $ nick <> ": " <> t

-- |
greet :: IrcAST -> Mind IRC ()
greet (Join nick name host chan) = void . forkMi $ do
    liftIO $ threadDelay (10^6*5)
    putPrivmsg chan "Hi"

-- |
adaptJoin :: IrcAST -> Mind IRC ()
adaptJoin (Join nick name host chan) = do
    host <- sees $ _servHost . _currServer
    dir <- _confDirectory <$> seeConfig
    musers <- join . fmap (Map.lookup host) <$> readConfig (dir <> "UserStats")

    let muser = join $ Map.lookup nick <$> musers
        stat = maybe Online id muser
        cinick = CI.mk nick

    adaptWith chan nick cinick name host $ Map.insert cinick . (userStatus .~ stat)

-- | Add a channel if it doesn't exist.
addJoin :: IrcAST -> Mind IRC ()
addJoin (Join nick name host chan) = do
    chans <- sees $ _servChannels . _currServer
    let cichan = CI.mk chan
    unless (Map.member cichan chans) $ do
        let chans' = Map.insert cichan (def & Lens.set chanName cichan) chans
        Tom.sets $ over currServer $ Lens.set servChannels chans'

-- }}}

-- {{{ Kick

-- TODO
-- XXX can `chans' and `nicks' really be several?
-- | Adapt the current from the Kick information.
adaptKick :: IrcAST -> Mind IRC ()
adaptKick (Kick nick name host chan nicks text) = do
    let cinicks = CI.mk nicks
    adaptWith chan nicks cinicks "" "" $
        Map.insert cinicks . over userChannels (Set.delete $ CI.mk chan)

-- | When the bot is kicked, rejoin if `chanAutoJoin' is `True'.
botKick :: IrcAST -> Mind IRC ()
botKick (Kick nick name host chans nicks text) = do
    edest <- sees _currDestination
    botnick <- sees $ _botNick . _servBot . _currServer
    let isNick = botnick == nicks
        e = flip fmap edest $ \c -> do
            when (_chanAutoJoin c && isNick) $ write "IRC" $ "JOIN " <> CI.original (_chanName c)
    either (warn . noChan . _userId) id e
  where
    noChan = ("Not a channel, " <>)

-- }}}

-- {{{ Mode

-- TODO
-- |
adaptMode :: IrcAST -> Mind IRC ()
adaptMode (Mode nick name host chan _ _) = do
    let cinick = CI.mk nick
    adaptWith chan nick cinick name host $ Map.insert cinick

-- TODO move `minus' and `plus' to Utils.
-- |
changeMode :: IrcAST -> Mind IRC ()
changeMode (Mode nick name host chan chars mtext) =
    mode chars $ maybe [] T.words mtext
  where
    usermodes = "vhoaq" :: String
    mode ('+':xs) ys = plus xs ys
    mode ('-':xs) ys = minus xs ys
    mode _ _ = return ()
    plus [] _ = return ()
    plus ('-':xs) ys = minus xs ys
    plus (x:xs) ys
        | any (== x) usermodes && length ys > 0 = do
            e <- modUser (CI.mk $ ys !! 0) $ \u ->
                let chans = _userChannels u
                    -- TODO FIXME
                    f = maybe (Just [x]) (Just . sort . (x :))
                    chans' = Set.insert (CI.mk chan) chans --Map.alter f chan chans
                in u { _userChannels = chans' }
            either warn return e
            plus xs $ tail ys
        | not $ any (== x) usermodes = do
            -- TODO
            e <- modChan (CI.mk chan) $ id --over chanMode (x:)
            either warn return e
            plus xs $ tailSafe ys
        | otherwise = warn "Missing MODE argument"
    minus [] _ = return ()
    minus ('+':xs) ys = plus xs ys
    minus (x:xs) ys
        | any (== x) usermodes && length ys > 0 = do
            e <- modUser (CI.mk $ ys !! 0) $ \u ->
                let chans = _userChannels u
                    -- TODO FIXME
                    f = maybe (Just []) (Just . filter (/= x))
                    chans' = Set.insert (CI.mk chan) chans --Set.alter f chan chans
                in u { _userChannels = chans' }
            either warn return e
            minus xs $ tail ys
        | not $ any (== x) usermodes  = do
            -- TODO FIXME
            e <- modChan (CI.mk chan) $ id -- over chanMode $ filter (/= x)
            either warn return e
            minus xs $ tailSafe ys
        | otherwise = warn "Missing MODE argument"

-- }}}

-- {{{ Nick

nickUserlist :: IrcAST -> Mind IRC ()
nickUserlist (Nick nick name host text) = do
    let citext = CI.mk text
        cinick = CI.mk nick
    adaptWith "" nick citext name host $ \u ->
        Map.insert citext (Lens.set userNick text u) . Map.delete cinick

-- }}}

-- {{{ Part

-- |
adaptPart :: IrcAST -> Mind IRC ()
adaptPart (Part nick name host chan _) = do
    let cinick = CI.mk nick
        cichan = CI.mk chan
    adaptWith chan nick cinick name host $
        Map.insert cinick . over userChannels (Set.delete cichan)

-- }}}

-- {{{ Privmsg

-- |
adaptPriv :: IrcAST -> Mind IRC ()
adaptPriv (Privmsg nick name host d _) = do
    let cinick = CI.mk nick
    adaptWith d nick cinick name host $ Map.insert cinick

-- | Respond to CTCP VERSION.
ctcpVersion :: IrcAST -> Mind IRC ()
ctcpVersion irc = do
    current <- see
    let t = privText irc
        c = either _userNick (CI.original . _chanName) $ _currDestination current
        v = "VERSION " <> version
    mwhen (t == "\SOHVERSION\SOH") $ putPrivmsg c $ ctcp v

-- TODO lookup user defined functions
-- |
runLang :: (Text -> Text -> Mind IRC ()) -> IrcAST -> Mind IRC ()
runLang send (Privmsg nick name host d t) = unlessBanned $ do
    fs <- Map.union funcs <$> serverfuncs funcs
    kl <- botparse fs t
    t <- compile fs kl
    send d t

-- |
printTell :: (Text -> Text -> Mind IRC ()) -> IrcAST -> Mind IRC ()
printTell send (Privmsg nick _ _ dest text) = unlessBanned $ do
    musers <- readLocalStored "tell"

    let cinick = CI.mk nick
        mtexts = join $ Map.lookup cinick <$> musers
        msgs = maybe [] id mtexts
        (msg, msgs') = pipeJoin msgs
        users = maybe (Map.singleton cinick msgs) (Map.insert cinick msgs') musers

    when (isJust msg) $ do
        modLocalStored "tell" $ const users
        send dest $ nick <> ", " <> fromJust msg

-- TODO prioritize locals
-- |
onMatch :: (Text -> Text -> Mind IRC ()) -> IrcAST -> Mind IRC ()
onMatch send irc@(Privmsg nick name host dest text) = unlessBanned $ do
    (mlon :: Maybe _) <- readLocalStored "respond"
    (msons :: Maybe (Map Text _)) <- readServerStored "respond"

    let mons = mlon <> (Map.unions . Map.elems <$> msons)
    let ons :: [(String, (Bool, Int, String))]
        ons = sortBy (comparing $ snd3 . snd) $ maybe mempty Map.toList mons

    void . decide $ forM_ ons $ \(match, (ins, n, resp)) -> deci . decide $ do
        let regex = mkRegexWithOpts match False ins

        emins <- try $ return $! matchRegex regex $ T.unpack text

        when (isLeft emins) $ do
            verb ("onMatch: " <> show emins)
            throwError ()

        let mins = either (const $ Just []) id emins

        unless (isJust mins) $ throwError ()

        let ins = fromJust mins
            -- FIXME who needs indexes larger than 9 anyway?!?!
            indexes = [ '\\' : show x | x <- [0 .. 9]]
            replacer = zipWith replace indexes (T.unpack text : ins)
            resp' = foldr ($) resp replacer

        lift $ runLang send $ irc { privText = T.pack resp' }
  where
    replace a b c = T.unpack $ T.replace (T.pack a) (T.pack b) (T.pack c)
    deci :: Mind IRC (Either () ()) -> Decide IRC () ()
    deci m = lift m >>= either return throwError

-- |
logPriv :: IrcAST -> Mind IRC ()
logPriv (Privmsg nick name host dest text) = do
    logPath <- _confLogPath <$> seeConfig
    time <- fmap T.pack . liftIO $ do
        formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" <$> getCurrentTime

    insertLog time text nick dest Null

-- }}}

-- {{{ Quit

quitUserlist (Quit nick name host text) = do
    let cinick = CI.mk nick

    adaptWith "" nick cinick name host $ Map.insert cinick . Lens.set userStatus Offline

-- }}}

-- {{{ Topic

-- XXX I think this is unnecessary
addTopic :: IrcAST -> Mind IRC ()
addTopic (Topic nick name host chan text) = do
    server <- sees _currServer

    let cs = _servChannels server
        cichan = CI.mk chan
        mc = Map.lookup cichan cs ^? _Just . to (Lens.set chanTopic text)
        ec = note ("No channel: " <> chan) mc
        cs' = either (const $ cs) (flip (Map.insert cichan) cs) ec

    Tom.sets $ over currServer $ Lens.set servChannels cs'

-- }}}

-- {{{ Invite

-- | Adapt the current from the Invite information.
adaptInv :: IrcAST -> Mind IRC ()
adaptInv (Invite nick name host dest chan) = do
    let cinick = CI.mk nick
    adaptWith chan nick cinick name host $ Map.insert cinick

-- | Join on invite.
joinInv :: IrcAST -> Mind IRC ()
joinInv (Invite nick name host dest chan) = unlessBanned $ do
    edest <- sees _currDestination
    let e = flip fmap edest $ \c -> do
        mwhen (_chanJoin c) $ write "IRC" $ "JOIN " <> chan
    either (warn . noChan . CI.original . _userId) id e
  where
    noChan :: Text -> Text
    noChan = ("Not a channel, " <>)

-- }}}

-- {{{ Numerics

-- |
adaptNum :: IrcAST -> Mind IRC ()
adaptNum (Numeric n ma t) = flip (maybe $ warn noArgs) ma $ \a -> do
    mchan <- Map.lookup (CI.mk a) <$> sees (_servChannels . _currServer)

    let user = Left def
        dest = maybe user Right mchan

    Tom.sets $ Lens.set currDestination dest
  where
    noArgs = "No `numArgs' in Numeric " <> n

-- TODO FIXME
-- | Cycle through the available nicks, and/or attempt to GHOST the prioritized
--   nick.
cycleNick :: IrcAST -> Mind IRC ()
cycleNick (Numeric n ma t) = warnDecide $ do
    server <- lift $ sees $ _servHost . _currServer
    cnick <- lift $ sees $ _botNick . _servBot . _currServer
    -- TODO Nickserv
    mnspw <- lift $ sees $ const Nothing

    lift $ do
        n <- T.pack . show <$> liftIO (randomRIO (0 :: Int, 999))
        Tom.sets $ over currServer $ over servBot $ over botNick (<> n)

    lift IRC.reconnect

    flip (maybe $ return ()) mnspw $ \nspw -> do
        nick <- lift . sees $ _botNick . _servBot . _currServer
        lift $ putPrivmsg "nickserv" $ "GHOST " <> nick <> " " <> nspw
        lift $ write "IRC" $ "NICK " <> nick

-- |
welcomeNum :: IrcAST -> Mind IRC ()
welcomeNum _ = do
    server <- sees _currServer

    let cs = map snd $ Map.toList $ _servChannels server

    forM_ cs $ \c -> when (_chanAutoJoin c) $ do
        write "IRC" $ "JOIN " <> CI.original (_chanName c)

    -- FIXME
    let mpass = Nothing --stServNickServId server
    write "IRC" $ "CAP REQ :multi-prefix"
    flip (maybe $ return ()) mpass $ \pass -> do
        putPrivmsg "NickServ" $ "identify " <> pass

-- |
whoisNum :: IrcAST -> Mind IRC ()
whoisNum (Numeric n ma t) = flip (maybe $ warn noArgs) ma $ \a -> do
    let xs = T.words a
        cinick = CI.mk $ atDef "" xs 0
        name = atDef "" xs 1
        host = atDef "" xs 2

    e <- modUser cinick $ \u ->
        let u' = Lens.set userName name u
            u'' = Lens.over userService (Lens.set IRC.userHost host) u'
        in u''

    either warn return e
  where
    noArgs = "No `numArgs' in Numeric " <> n

-- |
topicNum :: IrcAST -> Mind IRC ()
topicNum (Numeric n ma t) = void . decide $ do
    unless (isJust ma) $ lift (noNumeric "332") >> throwError ()
    lift $ modChanTopic (CI.mk $ fromJust ma) (const t) >>= verb

-- TODO a way to get the name and host of users on join
-- |
userlistNum :: IrcAST -> Mind IRC ()
userlistNum (Numeric n ma t) = do
    server <- sees _currServer
    dir <- _confDirectory <$> seeConfig
    let host = _servHost server
    flip (maybe $ noNumeric "353") ma $ \chan -> do
        musers <- join . fmap (Map.lookup host) <$> readConfig (dir <> "UserStats")
        forM_ (T.words t) $ \modenick -> do
            let (ircmode, nick) = T.break (`notElem` (id @String "~&@%+")) modenick
                cinick = CI.mk nick
                mode = toMode (T.unpack ircmode)
                users = _servUsers server
                cichan = CI.mk chan
                mu = mapChans (Set.insert cichan) <$> Map.lookup cinick users
                chans = Set.fromList [chan]
                susers = maybe mempty id musers
                stat = maybe Online id $ Map.lookup nick susers
                mu' = flip fmap mu $ Lens.set userStatus stat
                service = IRC.User "" ""
                user = fromJust $ mu' <|> Just (Bot.User nick "" cinick mempty stat service)
            modUserlist $ Map.insert cinick user
            when (stat > Online) $ return ()
                --putPrivmsg $ "nickserv status " <> CI.original nick

-- |
modeNum :: IrcAST -> Mind IRC ()
modeNum (Numeric n ma t) = flip (maybe $ warn noArgs) ma $ \a -> do
    let (chan, mode) = dropWhile (== '+') . T.unpack <$> bisect (== ' ') a
        cichan = CI.mk chan
    -- TODO FIXME
    e <- modChan cichan $ id --Lens.set chanMode mode
    either warn return e
    write "IRC" $ "MODE " <> a
  where
    noArgs = "No `numArgs' in Numeric " <> n

privilegeNum :: IrcAST -> Mind IRC ()
privilegeNum (Numeric n ma t) = flip (maybe $ warn noArgs) ma $ \a -> do
    write "IRC" $ "PRIVMSG " <> a <> " :" <> "Check my privileges please!"
  where
    noArgs = "No `numArgs' in Numeric " <> n

-- }}}

