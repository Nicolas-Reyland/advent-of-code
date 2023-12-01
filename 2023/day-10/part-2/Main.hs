module Main where
import Data.List.Split (chunksOf)

data Instruction = AddX Int | Noop deriving (Show)

-- (cycle count, value of X, pixels)
type CPUContext = (Int, Int, [Bool])

parseInput :: String -> [Instruction]
parseInput = (map parseLine) . lines
    where
        parseLine line = case line of
            "noop" -> Noop
            ('a':'d':'d':'x':' ':n) -> AddX $ read n

execInstruction :: CPUContext -> Instruction -> CPUContext
execInstruction (count, x, pixels) (AddX val) = (count + 2, x + val, pixel2 : pixel1 : pixels)
    where
        pixel1 = showablePixel (count + 1) x
        pixel2 = showablePixel (count + 2) x
execInstruction (count, x, pixels) Noop = let ncount = count + 1
                                          in (ncount, x, showablePixel ncount x : pixels)

showablePixel :: Int -> Int -> Bool
showablePixel count x = (abs $ ((count - 1) `mod` 40) - x) < 2

displayCRT :: [Bool] -> String
displayCRT b = unlines $ chunksOf 40 $ map (\p -> if p then '#' else '.') b

third :: (a, b, c) -> c
third (_,_,x) = x

main :: IO ()
main = interact $ displayCRT . reverse . third . (foldl execInstruction initial_ctx) . parseInput
    where
        initial_ctx = (0, 1, [])

