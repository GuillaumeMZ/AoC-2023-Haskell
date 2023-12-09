{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

import Common

stepsToReach :: T.Text -> T.Text -> BinTree T.Text -> Instructions -> Int
stepsToReach src dst tree instr =
    if src == dst then 0
    else case head instr of
            L -> 1 + stepsToReach (value leftSubTree) dst leftSubTree (tail instr)
            R -> 1 + stepsToReach (value rightSubTree) dst rightSubTree (tail instr)
    where leftSubTree  = leftChild tree
          rightSubTree = rightChild tree

main :: IO ()
main = do 
    input <- T.IO.getContents
    let lines = T.lines input
        instructions = instructify $ head lines
        tree = treeify "AAA" (recordify $ drop 2 lines)

    print $ stepsToReach "AAA" "ZZZ" tree (cycle instructions)