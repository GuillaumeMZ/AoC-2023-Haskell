module Common where

import           Data.Map (Map, (!))
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

data Move = L | R deriving (Show, Eq)

data BinTree a = Self | Branch a (BinTree a) (BinTree a) deriving(Show)

leftChild :: BinTree a -> BinTree a
leftChild tree@Self = tree 
leftChild (Branch _ l _) = l

rightChild :: BinTree a -> BinTree a
rightChild tree@Self = tree 
rightChild (Branch _ _ r) = r

value :: BinTree a -> a
value Self = undefined
value (Branch v _ _) = v

type Records = Map T.Text (T.Text, T.Text)
type Instructions = [Move]

treeify :: T.Text -> Records -> BinTree T.Text
treeify start records =
    let (left, right) = records ! start 
        leftSubTree   = treeify left records
        rightSubTree  = treeify right records
    in
    if left == right && left == start then Branch start Self Self
    else if left == start then Branch start Self rightSubTree
    else if right == start then Branch start leftSubTree Self
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