module Main where

f :: [Int] -> String
f ns = show $ length $ filter (uncurry (<)) $ zip ns (tail ns ++ [last ns])

main :: IO ()
main = interact $ f . map read . words

