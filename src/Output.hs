module Output where

import City
import Weather
import Data.Time
import Data.List

renderUTCTime :: UTCTime -> [Char]
renderUTCTime t = show (utctDay t) ++ "T" ++ clockTime ++ "Z"
    where totalSeconds = round $ utctDayTime t
          hours = totalSeconds `div` 3600
          hourRemainingSec = totalSeconds `mod` 3600
          minutes = hourRemainingSec `div` 60
          seconds = hourRemainingSec `mod` 60
          clockTime = show hours ++ ":" ++ show minutes ++ ":" ++ show seconds

data WeatherData = WeatherData IataCode Location UTCTime Condition Temperature Pressure Humidity

instance Show WeatherData where
    show (WeatherData i l u c t p h) =
        intercalate "|" [show i, show l, renderUTCTime u, show c, show t, show p, show h]
