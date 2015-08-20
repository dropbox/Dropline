{-# LANGUAGE OverloadedStrings #-} -- For HTML literals

module Dropline.Server (serve) where

import Dropline.Tracker (Signals)
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

serve :: TVar Signals -> IO ()
serve signals = do
    let conf = nullConf { port = 80 }
    let rssis = process signals
    simpleHTTP conf (raw rssis <|> friendly rssis)

raw :: ServerPart [RSSI] -> ServerPart Response
raw rssis = dir "raw" $ do
    signalData <- rssis
    let json = show [rssi | RSSI rssi <- signalData]
    ok $ toResponse json

friendly :: ServerPart [RSSI] -> ServerPart Response
friendly rssis = do
    signalData <- rssis
    ok $ toResponse $ H.html $ do
        H.head $ do
            H.title "Processed data"
        H.body $ do
            H.p $ H.toHtml (show signalData)

process :: TVar Signals -> ServerPart [RSSI]
process signals = do
    signals' <- liftIO . atomically . readTVar $ signals
    return $ map fst (elems signals') -- Take out only the RSSIs