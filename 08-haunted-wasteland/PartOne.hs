{-# LANGUAGE OverloadedStrings #-}

import           Data.Map (Map, (!))
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

data Move = L | R deriving (Show, Eq)

-- Empty should be named Self
data BinTree a = Empty | Branch a (BinTree a) (BinTree a) deriving(Show)

leftChild :: BinTree a -> BinTree a
leftChild tree@Empty = tree 
leftChild (Branch _ l _) = l

rightChild :: BinTree a -> BinTree a
rightChild tree@Empty = tree 
rightChild (Branch _ _ r) = r

value :: BinTree a -> a
value Empty = undefined

type Records = Map T.Text (T.Text, T.Text)
type Instructions = [Move]

treeify :: T.Text -> Records -> BinTree T.Text
treeify start records =
    let (left, right) = records ! start 
        leftSubTree  = treeify left records
        rightSubTree = treeify right records
    in
    if left == right && left == start then Branch start Empty Empty
    else if left == start then Branch start Empty rightSubTree
    else if right == start then Branch start leftSubTree Empty
    else Branch start leftSubTree rightSubTree

recordify :: [T.Text] -> Records
recordify input = 
    Map.fromList $ map (\line -> (T.take 3 line, (T.take 3 . T.drop 7 $ line, T.take 3 . T.drop 12 $ line))) input

parseMove :: Char -> Move
parseMove char
    | char == 'L' = L
    | char == 'R' = R
    | otherwise = undefined

instructify :: T.Text -> Instructions
instructify source = map parseMove (T.unpack source)

stepsToReach :: T.Text -> T.Text -> BinTree T.Text -> Instructions -> Int
stepsToReach src dst tree instr =
    if src == dst then 0
    else
        let curInstr = head instr in
        if curInstr == L then undefined
        else undefined

main :: IO ()
main = do 
    input <- T.IO.getContents
    let lines = T.lines input
    let instructions = instructify $ head lines
    let tree = treeify "AAA" (recordify $ drop 2 lines)

    print $ stepsToReach "AAA" "ZZZ" tree (cycle instructions)