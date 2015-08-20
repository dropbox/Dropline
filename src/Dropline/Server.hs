{-# LANGUAGE OverloadedStrings #-} -- For HTML literals

module Dropline.Server (serve) where

import Dropline.Tracker (Statuses, Status(..))
import Data.Time.Clock.POSIX (POSIXTime)
import Dropline.Wifi (RSSI(..))
import Control.Concurrent.STM (TVar, readTVar, atomically)
import Happstack.Server (Response)
import Happstack.Server.Routing (dir)
import Happstack.Server.SimpleHTTP (ServerPart,
    simpleHTTP, nullConf, port, toResponse, ok)
import qualified Text.Blaze.Html5 as H
import Control.Applicative ((<|>))
import Control.Monad.Trans (liftIO)
import Data.Map.Strict (elems)

serve :: TVar Statuses -> IO ()
serve signals = do
    let conf = nullConf { port = 80 }
    let rssis = process signals
    simpleHTTP conf (raw rssis <|> friendly rssis)

raw :: ServerPart [(RSSI, POSIXTime)] -> ServerPart Response
raw rssis = dir "raw" $ do
    signalData <- rssis
    ok $ toResponse (show signalData)

friendly :: ServerPart [(RSSI, POSIXTime)] -> ServerPart Response
friendly rssis = do
    signalData <- rssis
    let busyness = sum $ map score signalData
    ok $ toResponse $ H.html $ do
        H.head $ do
            H.title "Dropline busy-o-meter"
        H.body $ do
            H.p $ "It is"
            H.p $ H.h1 $ H.toHtml (show busyness)
            H.p $ "busy."

process :: TVar Statuses -> ServerPart [(RSSI, POSIXTime)]
process signals = do
    signals' <- liftIO . atomically . readTVar $ signals
    let format (Status rssi first last) = (rssi, last - first)
    return $ map format $ elems signals'

score :: (RSSI, POSIXTime) -> Double
score (RSSI signal, duration) = timeScore * signalScore
    where
    -- Starts to ignore signals around 2 or more hours old
    timeScore = (1/) . (1+) . exp . (/200) $ realToFrac (duration - 2*60*60)
    -- Starts to ignore signals below around 50 dBm
    signalScore = (1/) . (1+) . exp . (/10) $ realToFrac (negate signal - 50)