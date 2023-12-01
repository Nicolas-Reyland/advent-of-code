import Data.Char (digitToInt, isDigit)
import Data.List (stripPrefix, sort)
import Data.Maybe (fromJust, fromMaybe)

data Monkey = Monkey { identifier :: Int
                     , items :: [Integer]
                     , operation :: Operation
                     , test :: Integer
                     , success :: Int
                     , failure :: Int
                     , inspections :: Int
                     } deriving (Show)

data Operation = Operation OpValue OpOperator OpValue
data OpOperator = PlusOp | MultOp
type OpValue = Maybe Integer

instance Eq Monkey where
    (Monkey id1 _ _ _ _ _ _) == (Monkey id2 _ _ _ _ _ inspections2) = id1 == id2

instance Ord Monkey where
    (Monkey _ _ _ _ _ _ inspections1) `compare` (Monkey _ _ _ _ _ _ inspections2) =
        inspections1 `compare` inspections2

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
    in Monkey identifier starting_items operation test success failure 0

splitIntList :: String -> [Integer]
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

idFromMonkey :: Monkey -> Int
idFromMonkey (Monkey id_ _ _ _ _ _ _) = id_

monkeyFromId :: Int -> [Monkey] -> Monkey
monkeyFromId identifier = head . (filter ((==identifier) . idFromMonkey))

playRound :: [Monkey] -> [Monkey]
playRound ms = customFoldL playMonkey ms $ map idFromMonkey ms

playMonkey :: [Monkey] -> Int -> [Monkey]
playMonkey monkeys m_id = customFoldL (inspectItem m_id) monkeys [0..(length items - 1)]
    where (Monkey _ items _ _ _ _ _) = monkeyFromId m_id monkeys

inspectItem :: Int -> [Monkey] -> Int -> [Monkey]
inspectItem m_id monkeys item_index =
    throwItem new_monkeys m_id item_index new_item_wl (if runTest test new_item_wl then succ else fail)
        where
            (item_wl, monkey@(Monkey _ items op test succ fail _), new_monkeys) = extractItem m_id monkeys
            new_item_wl = runOperation op item_wl

extractItem :: Int -> [Monkey] -> (Integer, Monkey, [Monkey])
extractItem m_id monkeys = (item, new_monkey, replaceNth m_id new_monkey monkeys)
    where
        monkey@(Monkey id_ (item:items) op test succ fail inspections) = monkeyFromId m_id monkeys
        new_monkey = Monkey id_ items op test succ fail (inspections + 1)

runOperation :: Operation -> Integer -> Integer
runOperation (Operation val1 op val2) old =
    (case op of
        PlusOp -> (+)
        MultOp -> (*)
    ) (fromMaybe old val1) (fromMaybe old val2)

runTest :: Integer -> Integer -> Bool
runTest test value = value `mod` test == 0

throwItem :: [Monkey] -> Int -> Int -> Integer -> Int -> [Monkey]
throwItem monkeys m_id item_index item_wl dest_m_id =
        replaceNth dest_m_id new_dest_monkey monkeys
        where
            dest_monkey@(Monkey dest_id items dest_op dest_test dest_succ dest_fail dest_insp) = monkeyFromId dest_m_id monkeys
            new_dest_monkey = Monkey dest_id (items ++ [item_wl]) dest_op dest_test dest_succ dest_fail dest_insp

playGame :: Int -> [Monkey] -> [Monkey]
playGame 0 monkeys = monkeys
playGame n monkeys = playGame (pred n) $ playRound monkeys

monkeyBusiness :: Monkey -> Monkey -> Int
monkeyBusiness (Monkey _ _ _ _ _ _ inspections1) (Monkey _ _ _ _ _ _ inspections2) = inspections1 * inspections2

-- Main
main :: IO ()
main = interact $ show . (\(m1:[m2]) -> monkeyBusiness m1 m2) . (take 2) . reverse . sort . (playGame 1000) . parseInput

-- Utils
customFoldL :: (a -> e -> a) -> a -> [e] -> a
customFoldL _ a [] = a
customFoldL f a (x:xs) = customFoldL f (f a x) xs

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n - 1) newVal xs

removeNth :: Int -> [a] -> [a]
removeNth n l = take (n - 1) l ++ drop (n + 1) l
