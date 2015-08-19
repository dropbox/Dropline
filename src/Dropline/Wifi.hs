module Dropline.Wifi (Signal(..), MAC(..), RSSI(..)) where

import Data.ByteString (ByteString)

data Signal = Signal MAC RSSI deriving (Show)

newtype RSSI = RSSI Int deriving (Show)

newtype MAC = MAC ByteString deriving (Eq, Ord, Show)