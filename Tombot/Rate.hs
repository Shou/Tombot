
module Tombot.Rate (makeLimiter) where


import qualified Data.Vector as V

import System.Concurrent.STM
import System.IO.Unsafe (unsafePerformIO)



limiters :: TMVar (Vector Integer)
limiters = unsafePerformIO $ newTMVarIO mempty

makeLimiter :: Integer -> IO (Integer -> IO () -> IO ())
makeLimiter limit = do
    _ <- atomically $ do
        ls <- takeTMVar limiters
        let n = V.length ls
            ls' = V.insert limit ls
        putTMVar limiters ls'

