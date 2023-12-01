module Main where
import Text.Read (readMaybe)
import Data.Maybe (isJust, fromJust)

tail' :: [a] -> [a]
tail' [] = []
tail' l = tail l

subSums :: [Maybe Int] -> [Int]
subSums [] = []
subSums l = (sum $ fromJust <$> values) : (subSums $ tail' rest)
    where
        (values, rest) = span isJust l

main :: IO ()
main = interact $ show . maximum . subSums . (map readMaybe) . lines
