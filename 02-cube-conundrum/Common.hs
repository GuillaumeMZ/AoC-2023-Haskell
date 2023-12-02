{-# LANGUAGE OverloadedStrings #-}
module Common where

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

data CubeConfiguration =
    CubeConfiguration {
        red :: Int,
        green :: Int,
        blue :: Int
    } deriving(Show)

type Game = (Int, [CubeConfiguration])

tupleize :: [a] -> (a, a)
tupleize [v1, v2] = (v1, v2) 

parseCubeConfiguration :: T.Text -> CubeConfiguration
parseCubeConfiguration source = 
    let colorsMap = Map.fromList $ map (tupleize . reverse . T.words) (T.splitOn "," source)
    in CubeConfiguration {
        -- TODO: DRY
        red = (read :: String -> Int) . T.unpack . fromMaybe "0" . Map.lookup "red" $ colorsMap,
        green = (read :: String -> Int) . T.unpack . fromMaybe "0" . Map.lookup "green" $ colorsMap,
        blue = (read :: String -> Int) . T.unpack . fromMaybe "0" . Map.lookup "blue" $ colorsMap
    }

parseGame :: T.Text -> Game
parseGame source = 
    ((read :: String -> Int) . T.unpack $ fst parsedGame, map parseCubeConfiguration $ T.splitOn ";" $ snd parsedGame)
        where parsedGame = tupleize $ T.splitOn ":" (T.drop 5 source)