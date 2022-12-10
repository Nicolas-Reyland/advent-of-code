module Main where
import Data.List (nub)

data Direction = Up | Down | Left | Right deriving (Show)
data Move = Move Direction Int deriving (Show)

type Position = (Int, Int)
type PositionTuple = (Position, Position)

dirFromChar :: Char -> Direction
dirFromChar c = case c of
    'U' -> Up
    'D' -> Down
    'L' -> Main.Left
    'R' -> Main.Right

directionTuple :: Direction -> Position
directionTuple dir = case dir of
    -- swapping up and down, because our grid starts at the top, not the bottom
    -- it doesn't really matter...
    Up -> (0, 1)
    Down -> (0, -1)
    Main.Left -> (-1, 0)
    Main.Right -> (1, 0)

tupleAdd :: (Num a) => (a, a) -> (a, a) -> (a, a)
tupleAdd (a, b) (c, d) = (a + c, b + d)

parseInput :: String -> [Move]
parseInput = (map (\(dirc:' ':n) -> Move (dirFromChar dirc) (read n))) . lines

applyMoves :: PositionTuple -> [Move] -> [Position]
applyMoves (_, tailp) [] = [tailp]
applyMoves pos@(headp, tailp) (m:ms) = tails ++ applyMoves next_pos ms
    where
        all_next_pos = applyMove pos m
        next_pos = last all_next_pos
        tails = map snd $ init all_next_pos

applyMove :: PositionTuple -> Move -> [PositionTuple]
applyMove pos (Move dir count) = scanl applyDirection pos $ replicate count dir

applyDirection :: PositionTuple -> Direction -> PositionTuple
applyDirection (headp, tailp) dir = (next_headp, moveTail next_headp tailp)
    where
        next_headp = tupleAdd headp $ directionTuple dir

moveTail :: Position -> Position -> Position
moveTail headp@(hx, hy) tailp@(tx, ty) =
    let (vx, vy) = (hx - tx, hy - ty)
        (avx, avy) = (abs vx, abs vy)
    in if avx == 2 || avy == 2 then
        -- legal steps for the tail (normal vx, vy)
        let (nvx, nvy) = ((div' vx avx), (div' vy avy))
        -- move tail towards head
        in if vy == 0 then
            -- move left/right
            (tx + nvx, ty)
        else
            if vx == 0 then
                -- move up/down
                (tx, ty + nvy)
            else
                -- move diagonally
                (tx + nvx, ty + nvy)
    else
        tailp

div' :: Integral n => n -> n -> n
div' 0 _ = 0
div' a b = div a b

main :: IO ()
main = interact $ show . length . nub . (applyMoves start) . parseInput
    where
        start = ((0, 0), (0, 0))
