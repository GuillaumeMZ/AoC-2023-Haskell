import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

import Common

parseRaces :: [Int] -> [Int] -> [Race]
parseRaces times records = map (uncurry Race) (zip times records)

parseInput :: [T.Text] -> [Race]
parseInput input = parseRaces times records where
    times = parseTextToIntList . drop 1 . T.words $ input !! 0
    records = parseTextToIntList . drop 1 . T.words $ input !! 1

main :: IO ()
main = do
    input <- T.IO.getContents
    let races = parseInput $ T.lines input
    print $ product . map (length . winningHoldTimes) $ races 