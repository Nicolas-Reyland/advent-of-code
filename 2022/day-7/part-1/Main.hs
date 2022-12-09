module Main where
import Data.List (elemIndex)

data Node = File String Int | Dir Directory Int deriving (Eq, Show);
type Directory = (String, [Node])

type Command = (String, [String])
type CommandContext = (Directory, [String])

newDir :: Directory -> Node
newDir d = Dir d 0

parseCommands :: [String] -> [Command]
parseCommands [] = []
parseCommands (command_line:rest) = command : parseCommands remaining
    where
        (content, remaining) = span (\line -> head line /= '$') rest
        command = (command_line, content)

executeCommand :: CommandContext -> Command -> CommandContext
executeCommand ctx command = case command of
    (('$':' ':'c':'d':' ':path), []) -> executeCdCommand ctx path
    ("$ ls", nodes) -> executeLsCommand ctx nodes

executeCdCommand :: CommandContext -> String -> CommandContext
executeCdCommand (d, stack) path = case path of
    "/" -> (d, [])
    ".." -> (d, tail stack)
    _ -> (d, path : stack)

executeLsCommand :: CommandContext -> [String] -> CommandContext
executeLsCommand (d, stack) nodes =
    let sd = (cwd, map mkNode nodes)
    in (insertSubDirectory d (reverse stack) sd, stack)
    where
        cwd = stackPeek stack

insertSubDirectory :: Directory -> [String] -> Directory -> Directory
insertSubDirectory (dname, dlist) [] sd@(sdname, sdlist) =
    if dname == sdname then
        (dname, dlist ++ sdlist)
    else
        (dname, newDir sd : dlist)
insertSubDirectory d@(dname, dlist) stack@(path:rest) sd =
    let new_dlist = replaceNth index (newDir $ insertSubDirectory nextd rest sd) dlist
    in (dname, new_dlist)
    where
        index = case (Just path) `elemIndex` (dirNames dlist) of
            Just i -> i
            Nothing -> error $ "No index for d = " ++ show d ++ ", stack = " ++ show stack ++ ", sd = " ++ show sd
        Dir nextd _ = dlist !! index

dirNames :: [Node] -> [Maybe String]
dirNames [] = []
dirNames (Dir (name, _) _:rest) = Just name : dirNames rest
dirNames (File _ _:rest) = Nothing : dirNames rest

mkNode :: String -> Node
mkNode ('d':'i':'r':' ':dname) = newDir (dname, [])
mkNode fileline = File name size
    where
        (size_s, raw_name) = span (/=' ') fileline
        size = read size_s
        name = tail raw_name

stackPeek :: [String] -> String
stackPeek [] = "/"
stackPeek s = head s

customFoldL :: (a -> e -> a) -> a -> [e] -> a
customFoldL _ a [] = a
customFoldL f a (x:xs) = customFoldL f (f a x) xs

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | n > 0 = x:replaceNth (n - 1) newVal xs
    | otherwise = error $ "Negative index: " ++ show n

showNode :: Int -> Node -> String
showNode lvl (File name size) = (replicate lvl '\t') ++ name ++ " (" ++ (show size) ++ ")\n"
showNode lvl (Dir (name, nodes) _) = (replicate lvl '\t') ++ "dir " ++ name ++ "\n" ++ (concat $ map (showNode (lvl + 1)) nodes)

setDirSizes :: Node -> Node
setDirSizes node@(Dir (dname, dlist) _) = Dir (dname, map setDirSizes dlist) $ nodeSize node
setDirSizes file@(File _ _) = file

nodeSize :: Node -> Int
nodeSize (Dir (_, nodes) _) = sum $ map nodeSize nodes
nodeSize (File _ size) = size

isDir :: Node -> Bool
isDir (Dir _ _) = True
isDir (File _ _) = False

minSize :: Node -> Int
minSize node@(Dir (_, nodes) dsize) =
    if dsize <= 100000 then
        dsize + (sum $ map minSize $ filter isDir nodes)
    else
        sum $ map minSize $ filter isDir nodes

main :: IO ()
main = interact $ show . minSize . setDirSizes . newDir . fst . (customFoldL executeCommand (("/", []), [])) . parseCommands . lines
