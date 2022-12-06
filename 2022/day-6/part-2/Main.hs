module Main where
import GHC.Data.List.SetOps (hasNoDups)

messageStartPosition :: String -> Int
messageStartPosition stream =
    if hasNoDups $ take 14 stream then 14 else (+) 1 $ messageStartPosition $ tail stream

main :: IO ()
main = interact $ show . messageStartPosition
