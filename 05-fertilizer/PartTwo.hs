{-# LANGUAGE OverloadedStrings #-}
import           Data.Maybe (fromMaybe)
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

main :: IO ()
main = do
    input <- T.IO.getContents
    let almanac = parseAlmanac . T.splitOn "\n\n" $ input
    let almanac' = almanac { seeds = reparseSeeds (seeds almanac) }
    print $ minimum $ map (seedToLocation almanac') (seeds almanac')
    