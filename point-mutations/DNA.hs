module DNA where

hammingDistance :: String -> String -> Int
hammingDistance = ((length . filter id) .) . zipWith (/=)
