module Main where

import City
import Weather
import Output
import Model
import Control.Monad

main :: IO ()
main = do
    let towns = [Sydney .. Dubbo]
    ts <- timeSeries
    let stationData = map (makeStationData ts) towns
    mapM_ (mapM_ print) stationData

