module Dropline.Tracker (track, Signals) where

import Dropline.Wifi (Signal(..), MAC, RSSI)
import Dropline.Util (periodically)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TChan, newTChan, writeTChan, readTChan,
    TVar, newTVar, modifyTVar, readTVar, writeTVar, atomically)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq, (<|))
import qualified Data.Sequence as Seq
import Control.Monad (forever, when)
import Data.Foldable (foldl')

-- A map from a MAC address to its signal strength and when it was last seen
type Signals = Map MAC (RSSI, POSIXTime)
type DeleteQueue = Seq (MAC, POSIXTime)

track :: TChan Signal -> IO (TVar Signals)
track signals = do
    visible     <- atomically (newTVar Map.empty)
    deleteQueue <- atomically (newTVar Seq.empty)
    forkIO $ forever $ do
        Signal mac rssi <- atomically (readTChan signals)
        time <- getPOSIXTime
        atomically $ modifyTVar visible     (Map.insert mac (rssi, time))
        atomically $ modifyTVar deleteQueue ((mac, time) <|)
    forkIO $ periodically (reap visible deleteQueue)
    return visible

reap :: TVar Signals -> TVar DeleteQueue -> IO ()
reap visible deleteQueue = do
    currentTime <- getPOSIXTime
    -- If a signal was recorded over a minute ago, we should try to delete 
    let old = currentTime - 60.0 
    atomically $ do
        dq  <- readTVar deleteQueue
        vis <- readTVar visible
        let expired   = Seq.takeWhileR (\(mac, time) -> time <= old) dq
        let unexpired = Seq.dropWhileR (\(mac, time) -> time <= old) dq
        let vis' = foldl' remove vis expired
        writeTVar deleteQueue unexpired
        writeTVar visible     vis'
        where 
        remove vis (mac, time) = case Map.lookup mac vis of
            Just (rssi, rxTime) | rxTime == time -> Map.delete mac vis
            _                                    -> vis
