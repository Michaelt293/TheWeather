module Parser where

import Prelude hiding (takeWhile)
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Control.Applicative
import Data.Time
import City
import Weather

data Sign = Positive | Negative deriving (Show, Eq, Ord)

sign = do
    neg <- optional (char '-')
    return $ case neg of
                  Nothing -> Positive
                  Just _ ->  Negative

float = do
    sign' <- sign
    whole <- many digit
    point <- optional (char '.')
    fraction <- many digit
    return $ case point of
                  Nothing ->  convert sign' whole :: Double
                  Just _  -> convert sign' (whole ++ "." ++ fraction) :: Double

convert s str = case s of
                     Negative -> negate (read str)
                     Positive -> read str

latP = Lat <$> float

longP = Long <$> float

coordinateP = do
    lt <- latP
    char ','
    ln <- longP
    return $ Coordinate lt ln

elevationP :: Parser Elevation
elevationP = read <$> (many digit)

type Hr = Double
type Min = Double
type Sec = Double

data ParsedTime = ParsedTime Day Hr Min Sec deriving (Show, Eq, Ord)

{-
timeP = do
    date <- takeWhile (\x -> x /= 'T')
    char 'T'
    hr <- many digit
    char ':'
    min <- many digit
    char ':'
    sec <- many digit
    char 'Z'
    return $ ParsedTime (read date :: Day) (read hr :: Hr) (read min :: Min) (read sec :: Sec)

totalSec = show $ read hr * 3600 + read min * 60 + read sec
-}


temperatureP = Celsius <$> float

use = parseOnly
