module Common where

import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.Char (isDigit)

data Number = Number {
    value :: Int,
    row :: Int,
    startColumn :: Int,
    endColumn :: Int
} deriving (Show)

data Symbol = Symbol {
    srow :: Int,
    column :: Int,
    symbol :: Char
} deriving(Show)

(>=<) :: Int -> (Int, Int) -> Bool
value >=< (lower, higher) = value >= lower && value <= higher

isNumberNearSymbol :: Number -> Symbol -> Bool
isNumberNearSymbol number symbol =
    abs (srow symbol - row number) <= 1 && (column symbol >=< (startColumn number - 1, endColumn number + 1))

parseLine :: T.Text -> Int -> Int -> ([Number], [Symbol])
parseLine line row column =
    -- Use a fold here ?
    let (startingPoints, restAfterPoints) = T.span (=='.') line
        (startingNumber, restAfterNumber) = T.span isDigit line
        (symbol, restAfterSymbol)         = (T.take 1 line, T.drop 1 line)
    in if not $ T.null startingPoints then parseLine restAfterPoints row (column + T.length startingPoints)
       else if not $ T.null startingNumber then let (followingNumbers, followingSymbols) = parseLine restAfterNumber row (column + T.length startingNumber)
                                              in (Number {value = (read :: String -> Int) . T.unpack $ startingNumber, row = row, startColumn = column, endColumn = (column + T.length startingNumber) - 1} : followingNumbers, followingSymbols)
       else if not $ T.null symbol then let (followingNumbers, followingSymbols) = parseLine restAfterSymbol row (column + 1)
                                              in (followingNumbers, Symbol {srow = row, column = column, symbol = T.head symbol} : followingSymbols)
       else ([], [])

-- maybe expose a simpler function like
-- parseInput :: T.Text -> ([Number], [Symbol])
-- which calls parseNumbersAndSymbols (T.lines input) 1
parseNumbersAndSymbols :: [T.Text] -> Int -> ([Number], [Symbol])
parseNumbersAndSymbols (x:xs) row = 
    -- Use a fold here ?
    let (lineNumbers, lineSymbols) = parseLine x row 1
        (otherNumbers, otherSymbols) = parseNumbersAndSymbols xs (row + 1)
    in (lineNumbers ++ otherNumbers, lineSymbols ++ otherSymbols)
parseNumbersAndSymbols [] _ = ([], [])