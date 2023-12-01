import Common

main :: IO ()
main = do
    input <- getContents
    print $ sum . map calibration . words $ input