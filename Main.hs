
-- This file is part of Tombot, licensed under the GNU GPL 2 license.
-- See the file "LICENSE" for more information.
-- Copyright Shou, 2013

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where


-- {{{ Imports
import Config
import Tombot.Bot
import Tombot.IRC
import Tombot.Types
import Tombot.Utils

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.State

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.IO
-- }}}

-- XXX
-- - We are going to rename the bot?
--      - A name without `bot' in it.
-- - Store data as pure Haskell code in files, specifically for the Funcs.
--      - This means we don't need to make parsers and whatnot, we already have
--        GHC for that through Language.Haskell.Interpreter.
--      - There are no drawbacks other than requiring GHC to be installed, I
--        think?
--      - We can store functions there as well, which is great.
-- - Look for patterns and repetitions and make generic functions.

-- TODO
-- - Functions
--      - About
--      - Del
--      - .r or .reload for reloading modules.
-- - Logging
--      - Now just review it.
-- - Privilege system
--      - In progress
--          - Just need to review the functions
-- - Rejoin on Handle error
--      - Search for hPutStr and the likes
--      - Close Handles on reconnection.
--          - Make sure no Handle errors/reconnects are mistaken, such as the
--            reconnect loop one because two Eithers were joined.
-- - Add quit and part messages to config data
-- - UTF-8 encoding
--      - Now just review this.
-- - Personality/mood
-- - Run anything the bot writes through the parser. Or at least user commands.
--      - This means changes are made when the bot does something, such as NICK
--      - We've already set the bot up to be able to do this! `respond'!
-- - Message spam limit.
--      - Make a function that can only write 480 bytes of text to a Handle.
--      - Only n messages per m seconds.
--          - 5 messages per 3 seconds, drop everything after.
-- - Keep track of UserStat
--      - Now just review this.
-- - Save lines to a file.
--      - This will allow users to do something like:
--        :on /abc(d)/ :load file -> ra
--          - This also means we have to replace the matches (\1, \2, ...)
--            AFTER, not before, we've compiled the kawaiilang.
--      - How to delete?
-- - Separate `help' file for functions defined by `:let'
--      - Local to channel
--      - Save it in same file as the funcs, "letfuncs"
-- - Separate `funcs' list for user defined functions.
--      - Write the `funcs' to a file "letfuncs" and then if it exists there,
--        the function is user defined. This is local to the channel.
--      - Local to channel
-- - Deprecate the Config import, instead opting for plaintext and/or command
--   line arguments and shape Config from that.
-- - Kanji lookup function

-- FIXME
-- - handle Handle errors and rejoin.
-- - Add/modify channels on join
--      - Default Channel funcs.
--          - We don't want her to be completely useless in a channel, but no
--            dangerous funcs either; specifically network ones.
-- - Load UserStat from file.
--      - Now we just need to be able to set it somewhere.
-- - Make a function `kawaiiparse' that lessens the amount of code; we've copy-
--   pasted the same KawaiiLang compiling code all over the place.


main :: IO ()
main = do
    configt <- newTMVarIO $ toStConf config
    forM_ servers $ \server -> forkIO $ initialise configt server
    userInput configt

-- | Direct input
userInput :: TMVar StConfig -> IO ()
userInput ct = loop $ do
    line <- liftIO T.getLine
    let (server, rest) = bisect (== ' ') line
        (chan, mesg) = bisect (== ' ') rest
    servs <- fmap stConfServs . liftIO . atomically $ readTMVar ct
    let mserv = M.lookup (T.unpack server) servs
    when (mserv /= Nothing) $ put mserv
    let message = maybe rest (const mesg) mserv
        channel = maybe server (const chan) mserv
    ms <- get
    flip (maybe $ return ()) ms $ \st -> void . liftIO . flip runStateT st $ do
        let irc = Privmsg "Tombot" "" "" channel message
        adaptPriv irc
        let user = User "Tombot" "Tombot" "botnet.fbi.gov" Root M.empty
        sets $ \c -> c { currUser = user }
        runLang irc
  where
    loop :: StateT (Maybe (TMVar Current)) IO () -> IO ()
    loop m = void . flip runStateT Nothing $ forever $ m

