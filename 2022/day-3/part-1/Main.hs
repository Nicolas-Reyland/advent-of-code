module Main where

import Data.List (intersect)
import Data.Char (ord, isLower)

findItem  :: String -> Char
findItem s = head $ intersect s1 s2
    where
        (s1, s2) = splitAt ((length s + 1) `div` 2) s

charPriority :: Char -> Int
charPriority c
    | isLower c = ord c - ord 'a' + 1
    | otherwise = ord c - ord 'A' + 27

main :: IO ()
main = interact $ show . sum . (map $ charPriority . findItem) . lines
