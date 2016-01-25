module Model where

import City
import Weather
import Output
import Data.Time

tropicCapricorn :: Lat
tropicCapricorn = Lat (-23.4372)

nominalHr :: NominalDiffTime
nominalHr = 3600

nominalDay :: NominalDiffTime
nominalDay = nominalHr * 24

-- time of peak heating
peakHeating :: Long -> NominalDiffTime
peakHeating l = 4 * nominalHr + (115 - realToFrac (long l)) * 200

type NumberOfDays = Integer

-- days from the start of the year
daysFromStart :: UTCTime -> NumberOfDays
daysFromStart d = diffDays (utctDay d) firstDay
    where getYear (y, _, _) = y
          gregorianYr = (getYear . toGregorian . utctDay) d
          firstDay = fromGregorian gregorianYr 1 1

-- cosine curve to model the change of seasons
seasonalVariation :: NumberOfDays -> Double
seasonalVariation d = cos $ (fromIntegral d) * 2 * pi / 365

-- cosine curve to model change in temperature during the day
diurnalVariation :: (Floating r, Real a) => a -> Long -> r
diurnalVariation t ln = cos $ (realToFrac t - realToFrac (peakHeating ln)) *
                            2 * pi / (realToFrac nominalHr * 24) * 10

-- temperature profile from 25C to 35C
tempProfile :: Real a => a -> Long -> Temperature
tempProfile t ln = Celsius $ (diurnalVariation t ln) * 5 + 30

deltaLatCapricorn lt = lat . abs $ lt - tropicCapricorn

latAdjustedTemp :: Real a => a -> Coordinate -> Temperature
latAdjustedTemp t (Coordinate lt ln) =
    if lt < tropicCapricorn
       then tempProfile t ln
       else tempProfile t ln - (Celsius (deltaLatCapricorn lt * 0.5))

--seasonalAjustedTemp :: Real a => NumberOfDays -> a -> Coordinate -> Temperature
seasonalAjustedTemp utc c@(Coordinate lt ln) =
    if lt < tropicCapricorn
       then latAdjustedTemp t c
       else latAdjustedTemp t c + Celsius ((seasonalVariation d - 1) * (deltaLatCapricorn lt * 0.25))
    where d = daysFromStart utc
          t = utctDayTime utc

timeSeries :: IO [UTCTime]
timeSeries = do
    t <- getCurrentTime
    let startTime = UTCTime (utctDay t) 0
    return $ take 28 [ addUTCTime (addTime * nominalHr) startTime | addTime <- [0, 6 .. ]]

makeStationData :: [UTCTime] -> City -> [WeatherData]
makeStationData ts s = [ WeatherData (iataCode s) (location s) t Sunny (seasonalAjustedTemp t (coordinate s)) (HectoPascal 1000) 58 | t <- ts ]
