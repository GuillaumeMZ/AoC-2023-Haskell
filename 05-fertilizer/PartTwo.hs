{-# LANGUAGE OverloadedStrings #-}
import           Data.Function ((&))
import           Data.Maybe (fromJust, fromMaybe, isJust, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

import Common

groupN :: Int -> [a] -> [[a]]
groupN _ [] = []
groupN n l = take n l : groupN n (drop n l)

toRange :: [Int] -> [Int]
toRange (start:count:_) = take count $ iterate (+1) start

reparseSeeds :: [Int] -> [Int]
reparseSeeds originalSeeds = 
    concat . map toRange . (groupN 2) $ originalSeeds

foldRange :: RangeMap -> RangeMap -> Maybe RangeMap
foldRange sourceRange destRange =
    let s1 = destinationStart sourceRange
        e1 = destinationEnd sourceRange
        s2 = sourceStart destRange
        e2 = sourceEnd destRange in
            if e1 < s2 || s1 > e2 then Nothing -- no overlap
            else if s1 >= s2 && e1 <= e2 then Just sourceRange { destinationStart = s1 - s2 + destinationStart destRange, destinationEnd = destinationEnd destRange - e2 + e1 } -- sourceRange is a subrange of destRange
            else if s1 < s2 && e1 > e2 then Just destRange { sourceStart = sourceStart sourceRange + e2 - e1, sourceEnd = sourceEnd sourceRange - s1 + s2 } -- destRange is a subrange of sourceRange
            else if s1 < s2 && e1 > s2 && e1 <= e2 then Just destRange { sourceStart = sourceStart sourceRange + s2 - s1, sourceEnd = sourceEnd sourceRange, destinationEnd = destinationEnd destRange - e2 + e1 } -- sourceRange is a left partial range of destRange
            else Just destRange { sourceStart = sourceStart sourceRange + (s2 - s1), sourceEnd = sourceEnd sourceRange - e1 + e2 } -- sourceRange is a right partial range of destRange

foldRangeOneToMany :: RangeMap -> [RangeMap] -> [RangeMap]
foldRangeOneToMany range otherRanges = mapMaybe (foldRange range) otherRanges

foldRangesManyToMany :: [RangeMap] -> [RangeMap] -> [RangeMap]
foldRangesManyToMany sourceRanges destRanges = concat . map (`foldRangeOneToMany` destRanges) $ sourceRanges


main :: IO ()
main = do
    input <- T.IO.getContents
    let almanac = parseAlmanac . T.splitOn "\n\n" $ input
    --let seeds = reparseSeeds (Common.seeds almanac)
    let finalMap = foldRangesManyToMany (seedToSoil almanac) (soilToFertilizer almanac)
                    & (`foldRangesManyToMany` (fertilizerToWater almanac))
                    & (`foldRangesManyToMany` (waterToLight almanac))
                   -- & (`foldRangesManyToMany` (lightToTemperature almanac))
                   -- & (`foldRangesManyToMany` (temperatureToHumidity almanac))
                   -- & (`foldRangesManyToMany` (humidityToLocation almanac))

    print finalMap
    --print $ minimum $ map (mapRanges finalMap) seeds 
    