module Main where
import Data.List (intersect)

tuple2 :: [a] -> (a, a)
tuple2 (x:[y]) = (x, y)

parseRange :: String -> ((Int, Int), (Int, Int))
parseRange line = ((read t11, read $ tail t12), (read t21, read $ tail t22))
    where
        (t1, t2) = span (/=',') line
        (t11, t12) = span(/='-') t1
        (t21, t22) = span(/='-') $ tail t2

rangeOverlap :: (Int, Int) -> (Int, Int) -> Bool
rangeOverlap (a1, a2) (b1, b2) = not . null $ intersect [a1..a2] [b1..b2]

main :: IO ()
main = interact $ show . sum . (map $ fromEnum . uncurry rangeOverlap . parseRange) . lines
