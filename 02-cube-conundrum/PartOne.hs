import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

import Common

isRoundValid :: CubeConfiguration -> CubeConfiguration -> Bool
isRoundValid validConfiguration currentConfiguration = 
    red currentConfiguration <= red validConfiguration &&
    green currentConfiguration <= green validConfiguration &&
    blue currentConfiguration <= blue validConfiguration

isGameValid :: Game -> CubeConfiguration -> Bool
isGameValid game validConfiguration = all (isRoundValid validConfiguration) (snd game)

main :: IO ()
main = do
    input <- T.IO.getContents

    let games = map parseGame $ T.lines input
    let validConfiguration = CubeConfiguration 12 13 14
    
    print $ sum [fst game | game <- games, isGameValid game validConfiguration]