{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module City where

import Numeric (showFFloat)

data City = Sydney
          | Brisbane
          | Melbourne
          | Hobart
          | Adelaide
          | Perth
          | Darwin
          | AliceSprings
          | Canberra
          | Cairns
          | Dubbo
          deriving (Eq, Read)

instance Show City where
    show Sydney       = "Sydney"
    show Brisbane     = "Brisbane"
    show Melbourne    = "Melbourne"
    show Hobart       = "Hobart"
    show Adelaide     = "Adelaide"
    show Perth        = "Perth"
    show Darwin       = "Darwin"
    show AliceSprings = "Alice Springs"
    show Canberra     = "Canberra"
    show Cairns       = "Cairns"
    show Dubbo        = "Dubbo"

data IataCode = SYD
              | BNE
              | MEL
              | HBA
              | ADL
              | PER
              | DRW
              | ASP
              | CBR
              | CNS
              | DBO
              deriving (Show, Eq, Read)

iataCode :: City -> IataCode
iataCode city = case city of
                     Sydney       -> SYD
                     Brisbane     -> BNE
                     Melbourne    -> MEL
                     Hobart       -> HBA
                     Adelaide     -> ADL
                     Perth        -> PER
                     Darwin       -> DRW
                     AliceSprings -> ASP
                     Canberra     -> CBR
                     Cairns       -> CNS
                     Dubbo        -> DBO

newtype Lat = Lat {lat :: Double} deriving (Num, Eq, Ord, Read)

newtype Long = Long {long :: Double} deriving (Num, Eq, Ord, Read)

instance Show Lat where
    show x = showFFloat (Just 2) (lat x) ""

instance Show Long where
    show x = showFFloat (Just 2) (long x) ""

data Coordinate = Coordinate {getLat :: Lat, getLong :: Long}
                deriving (Eq, Ord, Read)

coordinate :: City -> Coordinate
coordinate city =
    case city of
         Sydney       -> Coordinate (Lat (-33.946111)) (Long 151.177222)
         Brisbane     -> Coordinate (Lat (-27.384167)) (Long 153.1175)
         Melbourne    -> Coordinate (Lat (-37.673333)) (Long 144.843333)
         Hobart       -> Coordinate (Lat (-42.836111)) (Long 147.510278)
         Adelaide     -> Coordinate (Lat (-34.945))    (Long 138.530556)
         Perth        -> Coordinate (Lat (-31.940278)) (Long 115.966944)
         Darwin       -> Coordinate (Lat (-12.408333)) (Long 130.87266)
         AliceSprings -> Coordinate (Lat (-23.806667)) (Long 133.902222)
         Canberra     -> Coordinate (Lat (-35.306944)) (Long 149.195)
         Cairns       -> Coordinate (Lat (-16.885833)) (Long 145.755278)
         Dubbo        -> Coordinate (Lat (-32.216667)) (Long 148.574722)

instance Show Coordinate where
    show (Coordinate lat' long') = show lat' ++ "," ++ show long'

type Metre = Int

type Elevation = Metre

elevation :: City -> Elevation
elevation city = case city of
                      Sydney       -> 7
                      Brisbane     -> 4
                      Melbourne    -> 3
                      Hobart       -> 4
                      Adelaide     -> 7
                      Perth        -> 22
                      Darwin       -> 34
                      AliceSprings -> 596
                      Canberra     -> 629
                      Cairns       -> 3
                      Dubbo        -> 312

data Location = Location Coordinate Elevation deriving (Eq, Ord)

instance Show Location where
    show (Location c e) = show c ++ "," ++ show e
