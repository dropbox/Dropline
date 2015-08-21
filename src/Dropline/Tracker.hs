module Dropline.Tracker (track, Statuses, Status(..)) where

import Dropline.Wifi (Signal(..), MAC, RSSI)
import Dropline.Util (periodically)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (TChan, readTChan, atomically,
    TVar, newTVar, modifyTVar, readTVar, writeTVar)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq, (<|))
import qualified Data.Sequence as Seq
import Control.Monad (forever)
import Data.Foldable (foldl')

-- A map from a MAC address to its signal strength and when it was last seen
data Status = Status {rssi      :: RSSI, 
                      firstSeen :: POSIXTime,
                      lastSeen  :: POSIXTime }
type Statuses = Map MAC Status
type DeleteQueue = Seq (MAC, POSIXTime)

track :: TChan Signal -> IO (TVar Statuses)
track signalStream = do
    statuses    <- atomically (newTVar Map.empty)
    deleteQueue <- atomically (newTVar Seq.empty)
    forkIO $ process signalStream statuses deleteQueue
    forkIO $ periodically (reap statuses deleteQueue)
    return statuses

process :: TChan Signal -> TVar Statuses -> TVar DeleteQueue -> IO ()
process signals statuses deleteQueue = forever $ do
    Signal mac rssi <- atomically (readTChan signals)
    time <- getPOSIXTime
    let status' = Status rssi time time
    atomically $ modifyTVar statuses $ Map.insertWith update mac status'
    atomically $ modifyTVar deleteQueue ((mac, time) <|)
    where
    update (Status _ firstSeen _) (Status rssi _ lastSeen) = 
        Status rssi firstSeen lastSeen

reap :: TVar Statuses -> TVar DeleteQueue -> IO ()
reap signals deleteQueue = do
    currentTime <- getPOSIXTime
    -- If a signal was recorded over 5 minutes ago, we should try to delete
    let old = currentTime - 300.0 
    atomically $ do
        dq   <- readTVar deleteQueue
        sigs <- readTVar signals
        let isOld (mac, time) = time <= old
        let (expired, unexpired) = Seq.spanr isOld dq
        let sigs' = foldl' remove sigs expired
        writeTVar deleteQueue unexpired
        writeTVar signals     sigs'
        where 
        remove sigs (mac, time) = case Map.lookup mac sigs of
            Just (Status _ _ rxTime) | rxTime == time -> Map.delete mac sigs
            _                                         -> sigs
