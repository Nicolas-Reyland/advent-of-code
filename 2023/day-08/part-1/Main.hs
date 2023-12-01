module Main where
import Data.Char (digitToInt)

tuplePlus :: (Num a) => (a, a) -> (a, a) -> (a, a)
tuplePlus (a, b) (c, d) = (a + c, b + d)

visibleTrees :: [[Int]] -> [(Int, Int)]
visibleTrees m = filter (isVisible m) coords
    where
        coords = [(y-1, x-1) | y <- [1..(length m)], x <- [1..(length $ m !! 0)]]

isVisible :: [[Int]] -> (Int, Int) -> Bool
isVisible m p@(y, x) =
    if y == 0 || x == 0 || y == (length m) - 1 || x == (length (m !! 0)) - 1 then
        True
    else
        any id $ map ((all ((>) center)) . (map (treeHeight m))) [
            [(y',x) | y' <- [(y + 1)..((length m) - 1)]],
            [(y',x) | y' <- [0..(y - 1)]],
            [(y,x') | x' <- [(x + 1)..((length $ m !! 0) - 1)]],
            [(y,x') | x' <- [0..(x - 1)]]]
        where
            center = treeHeight m p

treeHeight :: [[Int]] -> (Int, Int) -> Int
treeHeight m (y, x) = (m !! y) !! x

parseInput :: String -> [[Int]]
parseInput = (map (map digitToInt)) . lines

main :: IO ()
main = interact $ show . length . visibleTrees . parseInput
