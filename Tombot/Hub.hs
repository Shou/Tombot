
module Tombot.Hub where


import Control.Monad.IO.Class

import Data.Text (Text)


class Send context where
    send :: MonadIO m => String -> context Text -> m ()

class Receive context where
    recv :: MonadIO m => m (context Text)

