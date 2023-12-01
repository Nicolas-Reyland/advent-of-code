module Main where
import Data.Char (digitToInt)

tuplePlus :: (Num a) => (a, a) -> (a, a) -> (a, a)
tuplePlus (a, b) (c, d) = (a + c, b + d)

scenicScores :: [[Int]] -> [Int]
scenicScores m = map (scenicScore m) coords
    where
        coords = [(y-2, x-2) | y <- [3..(length m)], x <- [3..(length $ m !! 0)]]

scenicScore :: [[Int]] -> (Int, Int) -> Int
scenicScore m p@(y, x) =
    foldl1 (*) $ map ((length . takeWhile' ((>) center)) . (map (treeHeight m))) [
        reverse [(y,x') | x' <- [0..(x - 1)]], -- left
        reverse [(y',x) | y' <- [0..(y - 1)]], -- up
        [(y,x') | x' <- [(x + 1)..((length $ m !! 0) - 1)]], -- right
        [(y',x) | y' <- [(y + 1)..((length m) - 1)]] -- down
    ]
    where
        center = treeHeight m p

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p l = prefix ++ additional
    where
        (prefix, rest) = span p l
        additional = case rest of
            [] -> []
            (x:_) -> [x]

treeHeight :: [[Int]] -> (Int, Int) -> Int
treeHeight m (y, x) = (m !! y) !! x

parseInput :: String -> [[Int]]
parseInput = (map (map digitToInt)) . lines

main :: IO ()
main = interact $ show . maximum . scenicScores . parseInput
