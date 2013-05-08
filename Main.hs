
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where


-- {{{ Imports
import Config
import Tombot.Funcs
import Tombot.Parser
import Tombot.Types
import Tombot.Utils

import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Concurrent.STM
import Control.Error hiding (left, right)
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
import qualified Data.Text.IO as T

import qualified Language.Haskell.Interpreter as H

import Network

import System.Exit
import System.IO
import System.Timeout
-- }}}

-- XXX
-- - We are going to rename the bot?
--      - A name without `bot' in it.
-- - How to get channel mode
-- - Store data as pure Haskell code in files, specifically for the Funcs.
--      - This means we don't need to make parsers and whatnot, we already have
--        GHC for that through Language.Haskell.Interpreter.
--      - There are no drawbacks other than requiring GHC to be installed, I
--        think?
--      - We can store functions there as well, which is great.

-- TODO
-- - Functions
--      - .\ for evaluation Haskell code. Use `timeout' with
--        `Language.Haskell.Interpeter' and remove all IO.
--      - .set for manipulating Config/Server.
--          - We can even make a simplified plaintext Config file and just feed
--            that to this function!
--      - .r or .reload for reloading modules.
-- - We export two values from Config, `servers' and `config'.
--      - new data that holds Config, Server and Current.
--      - Or data is in a TMVar? This way we could also keep the Handles in
--        there.
--          - When to re-read the TMVar? Wouldn't reading from it all the time
--            be a bit excessive? Or not...?
-- - Logging
-- - Privilege system
-- - Rejoin on Handle error
-- - Get channel mode; see XXX
-- - Add quit and part messages to data
-- - UTF-8 encoding
--      - #shrug
-- - Personality/mood
-- - Modify userlist on quit/part/join
-- - Close Handles on reconnection.
--      - Make sure no Handle errors/reconnects are in error, such as the
--        reconnect loop one because two Eithers were joined.


connecter :: Server -> Mind Handle
connecter server = liftIO $ connecter' 1
  where
    connecter' :: Int -> IO Handle
    connecter' n = E.handle (onerror n) $ do
        verb $ "Connection delay by " <> show n <> " seconds"
        let host = servHost server
            port = servPort server
        threadDelay (10^6 * n)
        mh <- timeout (10^6 * 10) $ do
            h <- connectTo host $ PortNumber $ fromIntegral port
            te <- mkTextEncoding "UTF-8//TRANSLIT"
            hSetEncoding h te
            hSetBuffering h LineBuffering
            hSetNewlineMode h (NewlineMode CRLF CRLF)
            T.hPutStrLn h $ T.concat ["NICK ", servBotNick server]
            T.hPutStrLn h $ T.concat [ "USER "
                                     , servBotName server
                                     , " 0 * :"
                                     , servBotName server
                                     ]
            return h
        maybe (connecter' $ inctime n) pure mh
    inctime n = min (n * 2) 300
    onerror :: Int -> SomeException -> IO Handle
    onerror n e = erro e >> connecter' (inctime n)

connect :: Mind ()
connect = do
    server <- gets $ keepServ
    verb $ "Connecting to " <> servHost server
    h <- connecter server
    t <- gets $ keepConfigTMVar
    liftIO $ atomically $ do
        x <- takeTMVar t
        putTMVar t $ M.insert (servHost server) h <$> x
    puts $ \k -> k { keepServ = server { servHandle = Just h } }

puts f = do
    x <- get
    put $ f x

onPing :: IRC -> (IRC -> Mind ()) -> Mind ()
onPing p@(Ping _) mind = mind p
onPing _ _ = pure ()

onNumeric :: Text -> IRC -> (IRC -> Mind ()) -> Mind ()
onNumeric t n@(Numeric num chan text) mind | t == num = mind n
onNumeric _ _ _ = pure ()

onInvite :: IRC -> (IRC -> Mind ()) -> Mind ()
onInvite i@(Invite _ _ _ _ _) mind = mind i
onInvite _ _ = pure ()

-- XXX default data for User if it's a REAL private message?
-- TODO put rest of data from Channel in Current, such as userlist
onPrivmsg :: IRC -> (IRC -> Mind ()) -> Mind ()
onPrivmsg p@(Privmsg nick name host d t) mind = do
    current <- gets keepCurrent
    mc <- gets $ note ("No channel: " <> d) . M.lookup d . servChans . keepServ
    let ec = flip fmap mc $ \chan -> do
        let eu :: Either Text User
            eu = note ("No user: " <> nick) $ M.lookup nick $ chanUsers chan
            current' = current { currDest = Right chan }
            ec' :: Either Text Current
            ec' = flip fmap eu $ \user -> current' { currUser = user }
            swapcur :: Current -> Mind ()
            swapcur c = puts $ \k -> k { keepCurrent = c }
        flip (either warn) ec' swapcur
    either warn (const $ mind p) ec
onPrivmsg _ _ = pure ()

-- TODO TMVar -- wat
initialise :: Mind ()
initialise = connect >> listen

-- XXX WOW THIS FUNCTION SUCKS GET A LIFE NERD
listen :: Mind ()
listen = forever $ do
    (Keeper server configt current) <- get
    let mh = servHandle server
    -- XXX we shouldn't do this ;_;
    unless (isJust mh) $ connect
    let h = fromJust mh
    -- TODO try
    -- XXX we can use monad join with Either on the try and parser
    let try :: IO a -> IO (Either SomeException a)
        try = E.try
    eline <- liftIO $ try $ T.hGetLine h
    -- TODO less ugly way; maybe IRC fail is not an error, but warning
    --      also `exToText' is ugly as well get out
    either (onerror h) (void . return) eline
    let e = join $ left exToText $ flip fmap eline $ \line -> do
        let mirc = A.maybeResult $ flip A.feed "" $ A.parse ircparser line
        note line mirc
    flip (either warn) e $ \irc -> do
        liftIO $ print irc
        onPing irc $ \(Ping t) -> do
            write $ "PONG :" <> t
        onPrivmsg irc parseBot
        onInvite irc $ \(Invite nick name host dest chan) -> do
            -- TODO check chanJoin
            let allowed m = m
            allowed $ write $ "JOIN " <> chan
        onNumeric "001" irc $ \_ -> do
            let cs = map snd $ M.toList $ servChans server
            forM_ cs $ \c -> when (chanAutoJoin c) $ do
                write $ "JOIN " <> chanName c
                let pass = servNickServId server
                putPrivmsg "NickServ" $ "identify " <> pass
        onNumeric "353" irc $ \(Numeric n ma t) -> do
            verb . T.words $ t
            -- TODO
            users <- fmap M.fromList $ forM (T.words t) $ \nick -> do
                let (tmode, nick') = T.span (`elem` "~@%&+") nick
                    toMode "@" = "o"
                    toMode "%" = "h"
                    toMode "~" = "q"
                    toMode "&" = "a"
                    toMode "+" = "v"
                    toMode _ = "/!\\" -- XXX ruh roh
                    mode = toMode tmode
                -- TODO UserStat
                return (nick', User nick' "" "" mode UserStat)
            let ea = note "No Numeric argument" ma
                ec = join $ flip fmap ea $ \a -> fmap (a,)
                                         $ note ("No channel: " <> a)
                                         $ M.lookup a $ servChans server
                e = flip fmap ec $ \(a, c) -> do
                    let cs = servChans server
                        cs' = M.insert a (c { chanUsers = users }) cs
                        server' = server { servChans = cs' }
                        curr = current { currUsers = users }
                    puts $ \k -> k { keepServ = server', keepCurrent = curr }
            either warn id e

  where
    exToText :: SomeException -> Text
    exToText = T.pack . show
    -- TODO close Handle
    -- FIXME hClose happens on non-Handle errors
    onerror h e = erro e >> liftIO (hClose h) >> connect

-- XXX better name for this
parseBot :: IRC -> Mind ()
parseBot (Privmsg nick name host d t) = do
    (Keeper server configt current) <- get
    funcs <- fmap (confFuncs . fst) $ liftIO $ atomically $ readTMVar configt
    let echan = note ("No channel: " <> d) $ M.lookup d $ servChans server
        e = flip fmap echan $ \chan -> do
        let parser = botparser (chanPrefix chan) (M.keys funcs)
            mkl = A.maybeResult . flip A.feed "" $ A.parse parser t
            me = flip fmap mkl $ \kl -> do
                t <- compile funcs kl
                putPrivmsg d t
        maybe (return ()) id me
    either warn id e

main :: IO ()
main = do
    configt <- newTMVarIO (config, mempty)
    forM_ servers $ \server -> forkIO $ do
        let current = Current { currServ = servHost server
                              , currUsers = M.empty
                              }
            keeper = Keeper server configt current
        void $ runStateT initialise keeper
    userInput Nothing configt

userInput :: Maybe String -> TMVar (Config, Map String Handle) -> IO ()
userInput mhost tmvar = do
    line <- getLine
    mhs <- fmap snd $ atomically $ readTMVar tmvar
    let splitter x acc | x `isPrefixOf` line =
            let n = length x
            in Just (take n line, drop n line)
        splitter _ acc = acc
        mserver = foldr splitter Nothing $ M.keys mhs
        mserver' = mserver <|> fmap (,line) mhost
    let m = flip fmap mserver' $ \(host, msg) -> do
        when ("QUIT" `isPrefixOf` dropWhile (== ' ') msg) $ do
            forM_ (M.toList mhs) $ \(_, h) -> do
                let quitmsg = "QUIT :Something un-cute happened. :c"
                hPutStrLn h quitmsg
                verb quitmsg
            exitWith ExitSuccess
        let mh = M.lookup host mhs
            msg' = dropWhile (== ' ') msg
        unless (null msg') $ do
            let md = flip fmap mh $ \h -> do
                hPutStrLn h msg'
                putStrLn $ "\x1b[0;32m" <> msg' <> "\x1b[0m"
            maybe (return ()) id md
        return $ Just host
    emhost' <- try $ maybe (return Nothing) id m
    userInput (join $ hush emhost') tmvar
  where
    try :: IO a -> IO (Either SomeException a)
    try = E.try

putPrivmsg d t = unless (T.null t) $ write $ "PRIVMSG " <> d <> " :" <> t

chanKawaiibot = Channel { chanName = "#kawaiibot"
                        , chanJoin = True
                        , chanAutoJoin = True
                        , chanPrefix = ".:!"
                        , chanFuncs = Blacklist []
                        }

rizon = Server { servHost = "irc.rizon.net"
               , servPort = 6667
               , servChans = M.fromList [("#kawaiibot", chanKawaiibot)]
               , servBotNick = "Tombot"
               , servBotName = "Tomboy"
               , servNickServId = "vm2dfx:wq"
               }

servers = [rizon]

