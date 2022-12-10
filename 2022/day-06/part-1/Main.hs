module Main where
import GHC.Data.List.SetOps (hasNoDups)

protocolStartPosition :: String -> Int
protocolStartPosition stream =
    if hasNoDups $ take 4 stream then 4 else (+) 1 $ protocolStartPosition $ tail stream

main :: IO ()
main = interact $ show . protocolStartPosition
