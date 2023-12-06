import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

data Race = Race { time :: Int, record :: Int } deriving(Show)

parseRaces :: [Int] -> [Int] -> [Race]
parseRaces times records = map (uncurry Race) (zip times records)

parseTextToIntList :: [T.Text] -> [Int]
parseTextToIntList source = map ((read :: String -> Int) . T.unpack) source

parseInput :: [T.Text] -> [Race]
parseInput input = parseRaces times records where
    times = parseTextToIntList . drop 1 . T.words $ input !! 0
    records = parseTextToIntList . drop 1 . T.words $ input !! 1

computeDistance :: Int -> Int -> Int
computeDistance raceDurationTime buttonHoldTime = buttonHoldTime * (raceDurationTime - buttonHoldTime)

winningHoldTimes :: Race -> [Int]
winningHoldTimes race = 
    filter (> record race) . map (computeDistance $ time race) $ [0..time race]

main :: IO ()
main = do
    input <- T.IO.getContents
    let races = parseInput $ T.lines input
    print $ product . map (length . winningHoldTimes) $ races 