module Main where
import Text.Read (readMaybe)
import Data.List (sort)
import Data.Maybe (isJust, fromJust)

tail' :: [a] -> [a]
tail' [] = []
tail' l = tail l

subSums :: [Maybe Int] -> [Int]
subSums [] = []
subSums l = (sum (fromJust <$> values)) : (subSums $ tail' rest)
    where
        (values, rest) = span isJust l

main :: IO ()
main = interact $ show . sum . (take 3) . reverse . sort . subSums . (map readMaybe) . lines
