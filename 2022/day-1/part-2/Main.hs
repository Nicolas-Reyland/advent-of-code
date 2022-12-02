module Main where
import Text.Read (readMaybe)
import Data.List (sort)

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

subSums :: [Maybe Int] -> [Int]
subSums [] = []
subSums (Nothing:rest) = subSums rest
subSums l@((Just _):_) = (sum ((\(Just x) -> x) <$> values)) : subSums rest
    where
        (values, rest) = span isJust l

main :: IO ()
main = interact $ show . sum . (take 3) . reverse . sort . subSums . (map readMaybe) . lines
