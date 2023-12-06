module Common where

import qualified Data.Text as T

data Race = Race { time :: Int, record :: Int } deriving(Show)

parseTextToIntList :: [T.Text] -> [Int]
parseTextToIntList source = map ((read :: String -> Int) . T.unpack) source

computeDistance :: Int -> Int -> Int
computeDistance raceDurationTime buttonHoldTime = buttonHoldTime * (raceDurationTime - buttonHoldTime)

winningHoldTimes :: Race -> [Int]
winningHoldTimes race = filter (> record race) . map (computeDistance $ time race) $ [0..time race]