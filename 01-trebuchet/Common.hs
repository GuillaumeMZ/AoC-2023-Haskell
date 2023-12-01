module Common where

import Data.Char (isDigit)

calibration :: String -> Int
calibration source =  
    read [head digits, last digits] 
        where digits = filter isDigit source