module Main where

main :: IO ()
main = interact $ (\ns -> show $ length $ filter (uncurry (<)) $ zip ns (tail ns ++ [last ns])) . (map read . words :: String -> [Int])
