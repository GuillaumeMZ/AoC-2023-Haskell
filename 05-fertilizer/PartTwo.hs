{-# LANGUAGE OverloadedStrings #-}

import           Data.Function ((&))
import           Data.Maybe (fromJust, fromMaybe, isJust, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

import Common

groupN :: Int -> [a] -> [[a]]
groupN _ [] = []
groupN n l = take n l : groupN n (drop n l)

toRange :: [Int] -> (Int, Int)
toRange (start:count:_) = (start, start + count - 1)

reparseSeeds :: [Int] -> [(Int, Int)]
reparseSeeds originalSeeds = map toRange . (groupN 2) $ originalSeeds

foldRange :: RangeMap -> RangeMap -> Maybe RangeMap
foldRange sourceRange destRange =
    let (s1, e1) = to sourceRange
        (s2, e2) = from destRange in
            -- no overlap
            if e1 < s2 || s1 > e2 then Nothing
            -- sourceRange is a subrange of destRange
            else if s1 >= s2 && e1 <= e2 then Just sourceRange { to = (s1 - s2 + fst (to destRange), snd (to destRange) - e2 + e1) }
            -- destRange is a subrange of sourceRange           
            else if s1 < s2 && e1 > e2 then Just destRange { from = (fst (from sourceRange) + s2 - s1, snd (from sourceRange) - e1 + e2) } 
             -- sourceRange is a left partial range of destRange
            else if s1 < s2 && e1 >= s2 && e1 <= e2 then Just RangeMap { from = (fst (from sourceRange) + s2 - s1, snd (from sourceRange)), to = (fst (to destRange), snd (to destRange) - e2 + e1) }
            -- sourceRange is a right partial range of destRange
            else Just RangeMap { from = (fst (from sourceRange), snd (from sourceRange) - e1 + e2), to = (fst (to destRange) + s1 - s2, snd (to destRange)) }

foldRangeOneToMany :: RangeMap -> [RangeMap] -> [RangeMap]
foldRangeOneToMany range otherRanges = mapMaybe (foldRange range) otherRanges

foldRangesManyToMany :: [RangeMap] -> [RangeMap] -> [RangeMap]
foldRangesManyToMany sourceRanges destRanges = concat . map (`foldRangeOneToMany` destRanges) $ sourceRanges

smallestValueInRange :: (Int, Int) -> (Int, Int) -> Maybe Int
smallestValueInRange (s1, e1) (s2, e2) 
    | e1 < s2 || s1 > e2 = Nothing
    | s1 >= s2 = Just s1
    | otherwise = Just s2

rangeToRangeMap :: (Int, Int) -> RangeMap
rangeToRangeMap tuple = RangeMap {from = tuple, to = tuple}

main :: IO ()
main = do
    input <- T.IO.getContents
    let almanac = parseAlmanac . T.splitOn "\n\n" $ input
    let secs = sections almanac
    let newSeeds = reparseSeeds (Common.seeds almanac)
    let finalMap = map rangeToRangeMap newSeeds
                   & (`foldRangesManyToMany` (secs !! 0))
                   & (`foldRangesManyToMany` (secs !! 1))
                   & (`foldRangesManyToMany` (secs !! 2))
                   & (`foldRangesManyToMany` (secs !! 3))
                   & (`foldRangesManyToMany` (secs !! 4))
                   & (`foldRangesManyToMany` (secs !! 5))
                   & (`foldRangesManyToMany` (secs !! 6))

    print $ minimum $ map (fst . to) finalMap 
    