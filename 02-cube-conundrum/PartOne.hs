{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.Text.Read (decimal)
import Data.Maybe (fromMaybe)

data CubeConfiguration =
    CubeConfiguration {
        red :: Int,
        green :: Int,
        blue :: Int
    } deriving(Show, Eq, Ord)

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


isRoundValid :: CubeConfiguration -> CubeConfiguration -> Bool
isRoundValid validConfiguration currentConfiguration = 
    red currentConfiguration <= red validConfiguration &&
    green currentConfiguration <= green validConfiguration &&
    blue currentConfiguration <= blue validConfiguration

isGameValid :: Game -> CubeConfiguration -> Bool
isGameValid game validConfiguration = all (isRoundValid validConfiguration) (snd game)

main :: IO ()
main = do
    input <- T.IO.getContents

    let games = map parseGame $ T.lines input
    let validConfiguration = CubeConfiguration 12 13 14;
    
    print $ sum [fst game | game <- games, isGameValid game validConfiguration]
    --print games