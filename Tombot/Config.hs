
{-# LANGUAGE TypeOperators, TypeApplications #-}

module Tombot.Config where


import Tombot.Types

import qualified Data.Aeson as Aes
import qualified Data.ByteString.Lazy as BL

import Control.Exception
import Control.Lens as Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Type.Operator


-- | Read config from the current path
loadConfig :: MonadIO m => m $ Maybe Config
loadConfig = liftIO $ do
    ejsonBS <- try @SomeException $ BL.readFile "Config.json"

    either print (const $ return ()) ejsonBS

    return . join . either (const Nothing) Just $ Aes.decode <$> ejsonBS

