module Main where
import Data.Char (isDigit, digitToInt)
import Data.List (tails)

findJust' :: (a -> Maybe b) -> [a] -> b
findJust' f (a:rest) = case f a of
    Just n -> n
    Nothing -> findJust' f rest

readAlphaDigit :: String -> Maybe Int
readAlphaDigit s = if (length s) /= 0 && (isDigit $ head s) then Just $ digitToInt $ head s else case s of
    ('o':'n':'e':_) -> Just 1
    ('t':'w':'o':_) -> Just 2
    ('t':'h':'r':'e':'e':_) -> Just 3
    ('f':'o':'u':'r':_) -> Just 4
    ('f':'i':'v':'e':_) -> Just 5
    ('s':'i':'x':_) -> Just 6
    ('s':'e':'v':'e':'n':_) -> Just 7
    ('e':'i':'g':'h':'t':_) -> Just 8
    ('n':'i':'n':'e':_) -> Just 9
    _ -> Nothing

findNumber :: String -> Int
findNumber s = ((*) 10 $ findJust' readAlphaDigit t) + (findJust' readAlphaDigit $ reverse t)
    where t = tails s

f :: [String] -> Int
f = sum . map findNumber

main :: IO ()
main = interact $ show . f . lines

