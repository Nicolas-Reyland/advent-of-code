module Main where

data Instruction = AddX Int | Noop deriving (Show)

-- (cycle count, value of X, signal strength)
type CPUContext = (Int, Int, Int)

parseInput :: String -> [Instruction]
parseInput = (map parseLine) . lines
    where
        parseLine line = case line of
            "noop" -> Noop
            ('a':'d':'d':'x':' ':n) -> AddX $ read n

execInstruction :: CPUContext -> Instruction -> CPUContext
execInstruction (count, x, ss) Noop = (ncount, x, nss)
    where
        ncount = count + 1
        nss = signalStrength (ncount, x, 0)
execInstruction ctx@(count, x, sss) (AddX val) = (count + 2, x + val, max nsss1 nsss2)
    where
        -- nsss1 (new signal strength sum 1)
        nsss1 = signalStrength (count + 2, x, 0)
        -- nsss2 (new signal strength sum 2)
        nsss2 = signalStrength (count + 1, x, 0)

signalStrength :: CPUContext -> Int
signalStrength (count, x, _) = (
    if count == 20 || (count - 20) `mod` 40 == 0 then
        count * x
    else
        0
    )

main :: IO ()
main = interact $ show . sum . (map (\(_,_,x) -> x)) . tail . (scanl execInstruction initial_ctx) . parseInput
    where
        initial_ctx = (0, 1, 0)

