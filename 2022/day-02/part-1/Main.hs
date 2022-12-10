module Main where

playRound :: String -> Int
playRound s = case s of
    "A X" -> 1 + 3
    "A Y" -> 2 + 6
    "A Z" -> 3 + 0
    "B X" -> 1 + 0
    "B Y" -> 2 + 3
    "B Z" -> 3 + 6
    "C X" -> 1 + 6
    "C Y" -> 2 + 0
    "C Z" -> 3 + 3

main :: IO ()
main = interact $ show . sum . (map playRound) . lines
