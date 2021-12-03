module Main where

import qualified Data.Text as T

f :: Int -> (Int, Int) -> [(String, Int)] -> (Int, Int)
f aim (h, d) ((instr,amount):rest) = case instr of
    "forward" -> f aim (h + amount, d + aim * amount) rest
    "up" -> f (aim - amount) (h, d) rest
    "down" -> f (aim + amount) (h, d) rest
f _ t [] = t

main :: IO ()
main = interact $ show . (uncurry (*)) . f 0 (0, 0) . map (\line -> let [instr,x] = words line in (instr, read x)) . lines

