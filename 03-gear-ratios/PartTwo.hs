import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

import Common

associatedNumbers :: [Number] -> Symbol -> [Number]
associatedNumbers numbers symbol = filter ((flip isNumberNearSymbol) symbol) numbers

tupleize :: [a] -> (a, a)
tupleize [v1, v2] = (v1, v2)

mult :: (Number, Number) -> Int
mult (a, b) = value a * value b

main :: IO ()
main = do
    input <- T.IO.getContents

    let (numbers, symbols) = parseNumbersAndSymbols (T.lines input) 1
    print $ sum .
            map (mult . tupleize) .
            filter ((==2) . length) .
            map (associatedNumbers numbers) . 
            filter ((=='*') . symbol) $ symbols