{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

data Card = Card {
    winningNumbers :: [Int],
    possessedNumbers :: [Int]
} deriving(Show)

tupleize :: [a] -> (a, a)
tupleize [v1, v2] = (v1, v2)

textToIntList :: T.Text -> [Int]
textToIntList source = map ((read :: String -> Int) . T.unpack) $ T.words source

parseCard :: T.Text -> Card
parseCard source =
    let (winningNumbersText, possessedNumbersText) = tupleize . T.splitOn "|" . T.drop 10 $ source in
        Card {winningNumbers = textToIntList winningNumbersText, possessedNumbers = textToIntList possessedNumbersText}

parseCards :: T.Text -> [Card]
parseCards source = map parseCard $ T.lines source

possessedWinningNumbers :: Card -> [Int]
possessedWinningNumbers card = filter (`elem` winningNumbers card) (possessedNumbers card) 

pointsWrtCount :: Int -> Int
pointsWrtCount count = if count == 0 then 0 else 2 ^ (count - 1)

main :: IO ()
main = do
    input <- T.IO.getContents
    print $ sum . map (pointsWrtCount . length . possessedWinningNumbers) $ parseCards input