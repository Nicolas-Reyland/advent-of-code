module Main where

import Data.List (intersect)
import Data.Char (ord, isLower)

chunksOf' :: Int -> [a] -> [[a]]
chunksOf' _ [] = []
chunksOf' n l = h : chunksOf' n t
    where
        (h, t) = splitAt n l

findBadge  :: [String] -> Char
findBadge (s1:s2:[s3]) = head $ intersect s1 $ intersect s2 s3

charPriority :: Char -> Int
charPriority c
    | isLower c = ord c - ord 'a' + 1
    | otherwise = ord c - ord 'A' + 27

main :: IO ()
main = interact $ show . sum . (map $ charPriority . findBadge) . (chunksOf' 3) . lines
