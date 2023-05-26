import Data.Char (isLower)
delAllUpper :: String -> String
delAllUpper = unwords . filter (any isLower) . words
