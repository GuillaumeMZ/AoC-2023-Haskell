{-# LANGUAGE OverloadedStrings #-}
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

import Common

main :: IO ()
main = do
    input <- T.IO.getContents
    let almanac = parseAlmanac . T.splitOn "\n\n" $ input
    print $ minimum $ map (seedToLocation almanac) (seeds almanac)
    