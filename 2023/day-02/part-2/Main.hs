module Main where

playRound :: String -> Int
playRound (x:' ':['X']) = 0 + case x of
    'A' -> 3
    'B' -> 1
    'C' -> 2
playRound (x:' ':['Y']) = 3 + case x of
    'A' -> 1
    'B' -> 2
    'C' -> 3
playRound (x:' ':['Z']) = 6 + case x of
    'A' -> 2
    'B' -> 3
    'C' -> 1

main :: IO ()
main = interact $ show . sum . (map playRound) . lines
