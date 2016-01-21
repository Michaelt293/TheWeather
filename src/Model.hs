module Model where

import City
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

diurnalVariation :: (Floating r, Real a) => a -> Long -> r
diurnalVariation t ln = cos $ (realToFrac t - realToFrac (peakHeating ln)) * 2 * pi / (realToFrac nominalHr * 24)

