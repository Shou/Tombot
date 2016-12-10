
{-# LANGUAGE OverloadedStrings #-}

module Tombot.Errors where


import Data.Monoid

left <\> right = left <> "\n" <> right

noFileInPath file = "No file '" <> file <> "' found in path."

noConfig = noFileInPath "Config.json"
         <\> "Parhaps you forgot to configure Tombot?"

