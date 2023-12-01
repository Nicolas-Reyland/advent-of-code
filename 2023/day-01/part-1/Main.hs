module Main where
import Data.Char (isDigit, digitToInt)
import Data.List (find)
import Data.Maybe (fromJust)

findNumber :: String -> Int
findNumber line = ((*) 10 $ toInt $ find isDigit line) + (toInt $ find isDigit $ reverse line)
    where
        toInt (Just a) = digitToInt a

f :: [String] -> Int
f = sum . map findNumber

main :: IO ()
main = interact $ show . f . lines

