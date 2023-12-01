import Common

digitify :: String -> String
digitify ('o':'n':'e':tl)         = "1" ++ digitify ("e" ++ tl)
digitify ('t':'w':'o':tl)         = "2" ++ digitify ("o" ++ tl)
digitify ('t':'h':'r':'e':'e':tl) = "3" ++ digitify ("e" ++ tl)
digitify ('f':'o':'u':'r':tl)     = "4" ++ digitify ("r" ++ tl)
digitify ('f':'i':'v':'e':tl)     = "5" ++ digitify ("e" ++ tl)
digitify ('s':'i':'x':tl)         = "6" ++ digitify ("x" ++ tl)
digitify ('s':'e':'v':'e':'n':tl) = "7" ++ digitify ("n" ++ tl)
digitify ('e':'i':'g':'h':'t':tl) = "8" ++ digitify ("t" ++ tl)
digitify ('n':'i':'n':'e':tl)     = "9" ++ digitify ("e" ++ tl)
digitify (hd:tl)                  = hd : digitify tl
digitify []                       = ""

main :: IO ()
main = do
    input <- getContents
    print $ sum . map (calibration . digitify) . words $ input