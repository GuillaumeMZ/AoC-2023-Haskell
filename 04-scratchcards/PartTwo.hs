import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

import Common

mapFirstN :: Int -> (a -> a) -> [a] -> [a]
mapFirstN n f l = map f (take n l) ++ drop n l

-- assuming len 1 == len 2
scratchcardTotal :: [Int] -> [Int] -> Int
scratchcardTotal [] [] = 0
scratchcardTotal (winCount:winRest) (cardCount:cardRest) = 
    cardCount + scratchcardTotal winRest computedCardRest where
        computedCardRest = mapFirstN winCount (cardCount+) cardRest

main :: IO ()
main = do
    input <- T.IO.getContents
    let cards = parseCards input
        possessedWinningNumbersCount = map (length . possessedWinningNumbers) cards

    print $ scratchcardTotal possessedWinningNumbersCount (take (length possessedWinningNumbersCount) $ iterate (*1) 1)