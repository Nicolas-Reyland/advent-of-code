import Data.Char (digitToInt, isDigit)
import Data.List (stripPrefix)
import Data.Maybe (fromJust, fromMaybe)

data Monkey = Monkey { identifier :: Int
                     , startingItems :: [Int]
                     , operation :: Operation
                     , test :: Int
                     , success :: Int
                     , failure :: Int
                     } deriving (Show)
data Operation = Operation OpValue OpOperator OpValue
data OpOperator = PlusOp | MultOp
type OpValue = Maybe Int

instance Show Operation where
    show (Operation val1 op val2) = showVal val1 ++ " " ++ show op ++ " " ++ showVal val2
        where
            showVal Nothing = "old"
            showVal (Just a) = show a

instance Show OpOperator where
    show PlusOp = "+"
    show MultOp = "*"

parseInput :: String -> [Monkey]
parseInput = (map parseMonkey) . groupLines . lines

groupLines :: [String] -> [[String]]
groupLines [] = []
groupLines l = group : groupLines rest'
    where
        (group, rest) = span (not . null) l
        rest' = if null rest then [] else tail rest

strictStripPrefix :: Eq a => [a] -> [a] -> [a]
strictStripPrefix prefix = fromJust . (stripPrefix prefix)

parseMonkey :: [String] -> Monkey
parseMonkey (m_id:start:op:tst:succ:fail:[]) =
    -- identifier
    let identifier = digitToInt $ head $ strictStripPrefix "Monkey " m_id
        -- items
        items_s = strictStripPrefix "  Starting items: " start
        starting_items = splitIntList items_s
        -- operation
        op_s = strictStripPrefix "  Operation: new = " op
        operation = parseOperation op_s
        -- test
        test_s = strictStripPrefix "  Test: divisible by " tst
        test = read test_s
        -- success
        success_s = strictStripPrefix "    If true: throw to monkey " succ
        success = read success_s
        -- failure
        failure_s = strictStripPrefix "    If false: throw to monkey " fail
        failure = read failure_s
    in Monkey identifier starting_items operation test success failure

splitIntList :: String -> [Int]
splitIntList [] = []
splitIntList (a:b:[]) = [read (a:b:"")]
splitIntList (a:b:',':' ':c) = read (a:b:"") : splitIntList c

parseOperation :: String -> Operation
parseOperation line =
    let (line', val1) = parseOpValue line
        (line'', op) = parseOpOperator line'
        (_, val2) = parseOpValue line''
    in Operation val1 op val2

parseOpValue :: String -> (String, OpValue)
parseOpValue s = case s of
    "old" -> ("", Nothing)
    ('o':'l':'d':' ':rest) -> (rest, Nothing)
    _ -> if all isDigit s then
            ("", Just $ read s)
        else
            let (num_s, (_:rest)) = span isDigit s
            in (rest, Just $ read num_s)

parseOpOperator :: String -> (String, OpOperator)
parseOpOperator ('+':' ':rest) = (rest, PlusOp)
parseOpOperator ('*':' ':rest) = (rest, MultOp)

runOperation :: Int -> Operation -> Int
runOperation old (Operation val1 op val2) =
    (case op of
        PlusOp -> (+)
        MultOp -> (*)
    ) (fromMaybe old val1) (fromMaybe old val2)

main :: IO ()
main = interact $ unlines . (map show) . parseInput
