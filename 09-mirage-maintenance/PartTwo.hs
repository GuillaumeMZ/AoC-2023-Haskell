import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

import Common

prediction :: [[Int]] -> Int
prediction (a:b:xs) = head a - prediction (b:xs)
prediction _ = 0

main :: IO ()
main = do
    input <- T.IO.getContents
    print $ sum . map (prediction . allSteps . parseIntList) $ T.lines input