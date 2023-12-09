import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

import Common

endsWith :: T.Text -> Char -> Bool
endsWith source suffix = suffix == (last . T.unpack $ source)

countStepsForGhost :: Char -> [BinTree T.Text] -> Instructions -> Int -> Int
countStepsForGhost end trees (i:is) counter =
    if all ((`endsWith` end) . value) trees then counter
    else case i of
        L -> countStepsForGhost end (map leftChild trees) is (counter + 1)
        R -> countStepsForGhost end (map rightChild trees) is (counter + 1)

main :: IO ()
main = do
    input <- T.IO.getContents
    let lines        = T.lines input
        instructions = instructify $ head lines
        records      = recordify $ drop 2 lines
        starts       = filter (`endsWith` 'A') . Map.keys $ records
        trees        = map (`treeify` records) starts

    print $ countStepsForGhost 'Z' trees (cycle instructions) 0
    --print starts