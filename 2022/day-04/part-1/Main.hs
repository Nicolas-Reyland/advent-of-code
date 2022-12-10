module Main where

tuple2 :: [a] -> (a, a)
tuple2 (x:[y]) = (x, y)

parseRange :: String -> ((Int, Int), (Int, Int))
parseRange line = ((read t11, read $ tail t12), (read t21, read $ tail t22))
    where
        (t1, t2) = span (/=',') line
        (t11, t12) = span(/='-') t1
        (t21, t22) = span(/='-') $ tail t2

rangeIncludes :: (Int, Int) -> (Int, Int) -> Bool
rangeIncludes (a1, a2) (b1, b2) = (a1 <= b1 && a2 >= b2) || (b1 <= a1 && b2 >= a2)

main :: IO ()
main = interact $ show . sum . (map $ fromEnum . uncurry rangeIncludes . parseRange) . lines
