module Main where

import qualified Data.Text as T

f :: (Int, Int) -> [(String, Int)] -> (Int, Int)
f (h, d) ((instr,amount):rest) = case instr of
    "forward" -> f (h + amount, d) rest
    "up" -> f (h, d - amount) rest
    "down" -> f (h, d + amount) rest
f t [] = t

main :: IO ()
main = interact $ show . (uncurry (*)) . f (0, 0) . map (\line -> let [instr,x] = words line in (instr, read x)) . lines

