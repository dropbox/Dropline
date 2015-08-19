module Dropline.Util (periodically) where

import Control.Monad (forever)
import Control.Concurrent (threadDelay)

periodically :: IO a -> IO b
periodically act = forever (act >> threadDelay (10^6))