module Main (main) where

import Dropline.Kismet (connect)
import Dropline.Tracker (track)
import Dropline.Server (serve)

main = do
    signals <- connect
    visible <- track signals
    serve visible
