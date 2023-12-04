import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

import Common

pointsWrtCount :: Int -> Int
pointsWrtCount count = if count == 0 then 0 else 2 ^ (count - 1)

main :: IO ()
main = do
    input <- T.IO.getContents
    print $ sum . map (pointsWrtCount . length . possessedWinningNumbers) $ parseCards input