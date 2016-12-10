
-- This file is part of Tombot, licensed under the GNU GPL 2 license.
-- See the file "LICENSE" for more information.
-- Copyright Shou, 2013

{-# LANGUAGE TypeApplications, FlexibleContexts, OverloadedStrings,
             OverloadedLists, TypeOperators
#-}

module Tombot.Connection (connections) where


-- {{{ Imports
import Control.Concurrent.STM.TQueue
import Control.Type.Operator

import Data.Text (Text)
import Data.Map (Map)

import System.IO.Unsafe (unsafePerformIO)
-- }}}


connections :: Map Text $ TQueue Text
connections = [ ("IRC", ircTQueue)
              , ("Discord", discordTQueue)
              , ("Discourse", discourseTQueue)
              ]

{-# NOINLINE ircTQueue #-}
ircTQueue = unsafePerformIO newTQueueIO

{-# NOINLINE discordTQueue #-}
discordTQueue = unsafePerformIO newTQueueIO

{-# NOINLINE discourseTQueue #-}
discourseTQueue = unsafePerformIO newTQueueIO

