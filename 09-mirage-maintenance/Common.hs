module Common where

import qualified Data.Text as T

nextStep :: [Int] -> [Int]
nextStep (a:b:xs) = (b - a) : nextStep (b : xs)
nextStep _ = []

allSteps :: [Int] -> [[Int]]
allSteps history = 
    if all (==0) history then [history]
    else history : allSteps (nextStep history)

parseIntList :: T.Text -> [Int]
parseIntList source = map ((read :: String -> Int) . T.unpack) . T.words $ source