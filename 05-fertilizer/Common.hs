module Common where

import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

data RangeMap = RangeMap {
    from :: (Int, Int),
    to :: (Int, Int)
} deriving(Show)

type Section = [RangeMap]
type Sections = [Section]

data Almanac = Almanac {
    seeds :: [Int],
    sections :: Sections
} deriving(Show)

parseTextToIntList :: T.Text -> [Int]
parseTextToIntList source = map ((read :: String -> Int) . T.unpack) $ T.words source

parseRangeMap :: T.Text -> RangeMap
parseRangeMap source = 
    let destinationStart = intList !! 0
        sourceStart      = intList !! 1
        rangeLength      = intList !! 2
    in RangeMap {
        from = (sourceStart, sourceStart + rangeLength - 1),
        to = (destinationStart, destinationStart + rangeLength - 1)
    }
    where intList = parseTextToIntList source

parseSeeds :: T.Text -> [Int]
parseSeeds source = parseTextToIntList (T.drop 7 source)

parseSection :: T.Text -> [RangeMap]
parseSection section = map parseRangeMap $ (tail . T.lines) section

parseAlmanac :: [T.Text] -> Almanac
parseAlmanac sections = Almanac {
    seeds = parseSeeds $ head sections,
    sections = map parseSection $ tail sections
}

(>=<) :: Int -> (Int, Int) -> Bool
value >=< (lower, higher) = value >= lower && value <= higher

mapRange :: RangeMap -> Int -> Maybe Int
mapRange rangeMap value = 
    if not $ value >=< from rangeMap then Nothing
    else Just $ fst (to rangeMap) + value - fst (from rangeMap)

mapRanges :: [RangeMap] -> Int -> Int
mapRanges [] value = value
mapRanges (x:xs) value = 
    fromMaybe (mapRanges xs value) (mapRange x value)

seedToLocation :: Almanac -> Int -> Int
seedToLocation almanac seed = foldl (flip mapRanges) seed (sections almanac)