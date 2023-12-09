import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

nextStep :: [Int] -> [Int]
nextStep (a:b:xs) = (b - a) : nextStep (b : xs)
nextStep _ = []

allSteps :: [Int] -> [[Int]]
allSteps history = 
    if all (==0) history then [history]
    else history : allSteps (nextStep history)

prediction :: [[Int]] -> Int
prediction (a:b:xs) = last a + prediction (b:xs) 
prediction _ = 0

parseIntList :: T.Text -> [Int]
parseIntList source = map ((read :: String -> Int) . T.unpack) . T.words $ source

main :: IO ()
main = do
    input <- T.IO.getContents
    print $ sum . map (prediction . allSteps . parseIntList) $ T.lines input