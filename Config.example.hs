
module Config where

import Tombot.Types
import Tombot.Funcs (funcs)

-- This is the configuration file for the IRC bot.
-- Make sure all directories exist, such as the confDir and confLogPath.


home = "/home/<username>/"

config = Config { confVerbosity = 2
                , confLogging = True
                , confLogPath = home ++ "irc/logs/"
                , confPath = "Config.hs"
                , confDir = home ++ "irc/"
                , confFuncs = funcs
                }

servers = [ freenode, rizon ]

freenode = Server { servHost = "irc.freenode.net"
                  , servPort = 6667
                  , servChans = [ chanKawaiibot
                                ]
                  , servBotNicks = ["Otenba"]
                  , servBotName = "Tombot"
                  , servNickServId = ""
                  }

chanKawaiibot = Channel { chanName = "#kawaiibot"
                        , chanJoin = True
                        , chanAutoJoin = True
                        , chanPrefix = ":"
                        , chanFuncs = Blacklist []
                        }

rizon = Server { servHost = "irc.rizon.net"
               , servPort = 6667
               , servChans = [ chanKawaiibot
                             , chanRandom
                             ]
               , servBotNicks = ["Otenba"]
               , servBotName = "Tombot"
               , servNickServId = ""
               }

chanKawaiibot = Channel { chanName = "#kawaiibot"
                        , chanJoin = True
                        , chanAutoJoin = True
                        , chanPrefix = ".:"
                        , chanFuncs = Blacklist []
                        }

chanRandom = Channel { chanName = "#asdjkd8a"
                     , chanJoin = True
                     , chanAutoJoin = False
                     , chanPrefix = ".:"
                     , chanFuncs = Whitelist [">", "help", "ra"]
                     }

