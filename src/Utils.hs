
module Utils where

import Data.Text

-- | Fills a list with "0" if the length of the list is too small.
fillList :: [String] -> [String]
fillList [a,b,c,d] = [a,b,c,d]
fillList [a,b,c]   = [a,b,c,"0"]
fillList [a,b]     = [a,b,"0","0"]
fillList [a]       = [a,"0","0","0"]
fillList []        = ["0","0","0","0"]

logString :: String -> IO ()
logString s = putStrLn s

logText :: Text -> IO ()
logText t = putStrLn $ unpack t
