{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

import Common

parseInput :: [T.Text] -> Race
parseInput source = Race { 
    time = (read :: String -> Int) . T.unpack . T.drop 1 . T.dropWhile (/= ':') . T.replace " " "" $ source !! 0 , 
    record = (read :: String -> Int) . T.unpack . T.drop 1 . T.dropWhile (/= ':') . T.replace " " "" $ source !! 1
}

-- TODO: optimize solution !
main :: IO ()
main = do
    input <- T.IO.getContents
    print $ length $ winningHoldTimes $ parseInput $ T.lines input