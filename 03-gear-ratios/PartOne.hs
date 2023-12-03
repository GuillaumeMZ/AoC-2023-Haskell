import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

import Common 

main :: IO ()
main = do
    input <- T.IO.getContents

    let (numbers, symbols) = parseNumbersAndSymbols (T.lines input) 1
    print $ sum [value number | number <- numbers, any (isNumberNearSymbol number) symbols]