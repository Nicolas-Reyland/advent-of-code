module Main where
import Text.Read (readMaybe)

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

subSums :: [Maybe Int] -> [Int]
subSums [] = []
subSums (Nothing:rest) = subSums rest
subSums l@((Just _):_) = (sum $ (\(Just x) -> x) <$> values) : subSums rest
    where
        (values, rest) = span isJust l

main :: IO ()
main = interact $ show . maximum . subSums . (map readMaybe) . lines
