import Data.Char (isDigit)

calibration :: String -> Int
calibration source =  
    read [head digits, last digits] 
        where digits = filter isDigit source

main :: IO ()
main = do
    input <- getContents
    print $ sum . map calibration . words $ input