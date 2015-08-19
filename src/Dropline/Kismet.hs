{-# LANGUAGE OverloadedStrings #-} -- Allows for bytestring literals

module Dropline.Kismet (connect) where

import qualified Network as Net
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (TChan, newTChan, writeTChan, atomically)
import Data.ByteString.Char8 (ByteString, 
    hGetLine, hPut, isPrefixOf, split, unpack)
import Control.Monad (forever, when)
import Dropline.Wifi (Signal(..), MAC(..), RSSI(..))

connect :: IO (TChan Signal)
connect = do
    kismet <- Net.connectTo "localhost" (Net.PortNumber 2501)
    hPut kismet "!0 REMOVE TIME\n" -- Stop sending heartbeats
    hPut kismet "!0 ENABLE CLIENT mac,signal_dbm\n" -- Subscribe to MAC data
    chan <- atomically newTChan
    forkIO $ forever $ do
        message <- hGetLine kismet
        when ("*CLIENT:" `isPrefixOf` message) (send message chan)
    return chan

send :: ByteString -> TChan Signal -> IO ()
send message chan = do
    let [_, addr, rssi, _] = split ' ' message
    let signal = Signal (MAC addr) (RSSI $ read $ unpack rssi)
    atomically (writeTChan chan signal)
