module Main where
import Data.List (stripPrefix, group, transpose)
import Data.Char (isNumber, isAlpha)
import Data.Maybe (fromJust, isNothing)

type Stack = [Char]
type Move = (Int, Int, Int)

parseLines :: [String] -> ([Stack], [Move])
parseLines l = (parseStacks stackLines, parseMoves moveLines)
    where
        (stackLinesRaw, moveLinesRaw) = span (not . null) l
        stackLines = init stackLinesRaw
        moveLines = tail moveLinesRaw

parseStacks :: [String] -> [Stack]
parseStacks l = map (
        fromJust . sequence . dropWhile isNothing
    ) $ transpose $ map parseStackLayer l

parseStackLayer :: String -> [Maybe Char]
parseStackLayer s = map (
    (\c ->
        case c of
            ' ' -> Nothing
            _ -> Just c
        ) . fst
    ) $ filter (\(_,n) -> n `mod` 4 == 0) $ zip (tail s) [0..]

parseMoves :: [String] -> [Move]
parseMoves = map parseMove

parseMove :: String -> Move
parseMove s0 = let s1 = fromJust $ stripPrefix "move " s0
                   (ns, s2) = span isNumber s1
                   n = read ns
                   s3 = fromJust $ stripPrefix " from " s2
                   (froms, s4) = span isNumber s3
                   from = read froms - 1 -- stack indexes should start at 0, not 1
                   s5 = fromJust $ stripPrefix " to " s4
                   (tos, s6) = span isNumber s5
                   to = read tos - 1 -- stack indexes should start at 0, not 1
                   in (n, from, to)

applyMove :: [Stack] -> Move -> [Stack]
applyMove stacks (n, from, to) =
    let old_from = stacks !! from
        new_to = (take n old_from) ++ (stacks !! to)
        new_from = drop n old_from
        in replaceNth from new_from $ replaceNth to new_to stacks

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n - 1) newVal xs

customFoldL :: (a -> e -> a) -> a -> [e] -> a
customFoldL _ a [] = a
customFoldL f a (x:xs) = customFoldL f (f a x) xs

parseMain :: String -> String
parseMain s = let (stacks, moves) = parseLines $ lines s
              in map head $ customFoldL applyMove stacks moves

main :: IO ()
main = interact $ parseMain

