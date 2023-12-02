import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

import Common

multiplyRGB :: CubeConfiguration -> Int
multiplyRGB CubeConfiguration {red, green, blue} = red * green * blue

bestConfiguration :: [CubeConfiguration] -> CubeConfiguration
bestConfiguration configurations = 
    CubeConfiguration {
        red = maximum $ map red configurations,
        green = maximum $ map green configurations,
        blue = maximum $ map blue configurations
    }

main :: IO()
main = do
    input <- T.IO.getContents

    print $ sum . map (multiplyRGB . bestConfiguration . snd . parseGame) $ T.lines input