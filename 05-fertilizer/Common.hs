module Common where

import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

data RangeMap = RangeMap {
    sourceStart :: Int,
    sourceEnd :: Int,
    destinationStart :: Int,
    destinationEnd :: Int
} deriving(Show)

data Almanac = Almanac {
    seeds :: [Int],
    seedToSoil :: [RangeMap],
    soilToFertilizer :: [RangeMap],
    fertilizerToWater :: [RangeMap],
    waterToLight :: [RangeMap],
    lightToTemperature :: [RangeMap],
    temperatureToHumidity :: [RangeMap],
    humidityToLocation :: [RangeMap]
} deriving(Show)

parseTextToIntList :: T.Text -> [Int]
parseTextToIntList source = map ((read :: String -> Int) . T.unpack) $ T.words source

parseRangeMap :: T.Text -> RangeMap
parseRangeMap source = 
    let destinationStart = intList !! 0
        sourceStart      = intList !! 1
        rangeLength      = intList !! 2
    in RangeMap {
        sourceStart = sourceStart,
        sourceEnd = sourceStart + rangeLength - 1,
        destinationStart = destinationStart,
        destinationEnd = destinationStart + rangeLength - 1
    }
    where intList = parseTextToIntList source

parseSeeds :: T.Text -> [Int]
parseSeeds source = parseTextToIntList (T.drop 7 source)

parseSection :: T.Text -> [RangeMap]
parseSection section = map parseRangeMap $ (tail . T.lines) section

parseAlmanac :: [T.Text] -> Almanac
parseAlmanac sections = Almanac {
    seeds = parseSeeds $ sections !! 0,
    seedToSoil = parseSection $ sections !! 1,
    soilToFertilizer = parseSection $ sections !! 2,
    fertilizerToWater = parseSection $ sections !! 3,
    waterToLight = parseSection $ sections !! 4,
    lightToTemperature = parseSection $ sections !! 5,
    temperatureToHumidity = parseSection $ sections !! 6,
    humidityToLocation = parseSection $ sections !! 7
}

(>=<) :: Int -> (Int, Int) -> Bool
value >=< (lower, higher) = value >= lower && value <= higher

mapRange :: RangeMap -> Int -> Maybe Int
mapRange rangeMap value = 
    if not $ value >=< (sourceStart rangeMap, sourceEnd rangeMap) then Nothing
    else Just $ destinationStart rangeMap + (value - sourceStart rangeMap)

mapRanges :: [RangeMap] -> Int -> Int
mapRanges [] value = value
mapRanges (x:xs) value = 
    fromMaybe (mapRanges xs value) (mapRange x value)

seedToLocation :: Almanac -> Int -> Int
seedToLocation almanac seed =
    (mapRanges $ humidityToLocation almanac) .
    (mapRanges $ temperatureToHumidity almanac) .
    (mapRanges $ lightToTemperature almanac) .
    (mapRanges $ waterToLight almanac) .
    (mapRanges $ fertilizerToWater almanac) .
    (mapRanges $ soilToFertilizer almanac) .
    (mapRanges $ seedToSoil almanac) $ seed