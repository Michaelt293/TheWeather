{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Weather where

import Numeric (showFFloat)
import City

data Condition = Sunny
               | Cloudy
               | Rain
               | Thunderstorm
               | Snow
               deriving (Show, Eq, Read)

newtype Temperature = Celsius {temp :: Double} deriving (Num, Eq, Ord, Read)

instance Show Temperature where
    show x = if temperature > 0
                then "+" ++ showFFloat (Just 1) temperature  ""
                else showFFloat (Just 1) temperature  ""
        where temperature = temp x

newtype Pressure = HectoPascal {pressure :: Double} deriving (Num, Eq, Ord, Read)

instance Show Pressure where
    show x = showFFloat (Just 1) (pressure x) ""

type Humidity = Int

adjustPressure :: Pressure -> Elevation -> Pressure
adjustPressure p e = HectoPascal $ (pressure p) - 0.1 * fromIntegral e

fromPotentialTemp :: Temperature -> Elevation -> Temperature
fromPotentialTemp t e = Celsius $ (temp t) - 9.8 * fromIntegral e
