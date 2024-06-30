module LambdaMan where

import Data.List.Extra (takeEnd, minimumOn, maximumOn, splitOn, elemIndex, findIndex)
import Data.Maybe (isJust, fromJust)
import qualified WebRepl
import Data.IntMap (IntMap)
import Data.IntMap qualified as M


------------------------------------------------
-- Refactor
------------------------------------------------

-- type Board' = IntMap

------------------------------------------------
-- Old method
------------------------------------------------

-- build a DFS tree from lambdaman's position
-- take the branch that has the least uneaten pellets

neighbors :: [(Int, Int)]
neighbors = [(1, 0), (-1, 0), (0, 1), (0, -1)]

set :: Int -> a -> [a] -> [a]
set i v vs = take i vs ++ [v] ++ takeEnd (length vs - i - 1) vs

set2 :: (Int, Int) -> a -> [[a]] -> [[a]]
set2 (x, y) v vs = set y (set x v (vs !! y)) vs

find2 :: Eq a => a -> [[a]] -> (Int, Int)
find2 v vs =
    let indices = map (elemIndex v) vs in
    let y = fromJust $ findIndex isJust indices in
        (fromJust $ indices !! y, y)

sumPos :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumPos (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

data Cell
    = Empty
    | Start
    | Pill
    | Wall
    deriving (Eq, Show)

type Board = [[Cell]]

-- keep track of whether a cell has been visited
type ExplorationBoard = [[Maybe Cell]]

toExploration :: Board -> ExplorationBoard
toExploration = map (map Just)

-- text representation of the board -> (the board, starting position)
strToBoard :: String -> (Board, (Int, Int))
strToBoard s =
    let b = map (map charToCell) $ filter (not . null) (splitOn "\n" s) in
        (b, find2 Start b)

charToCell :: Char -> Cell
charToCell 'L' = Start
charToCell '#' = Wall
charToCell '.' = Pill
charToCell ' ' = Empty -- for debugging
charToCell c = error $ "Invalid character '" ++ [c] ++ "' in Lambdaman board"

-- partially explored board -> position -> number of pellets
checkBranch :: ExplorationBoard -> (Int, Int) -> Int
checkBranch board pos@(x, y) =
    if (x < 0 || y < 0 || x >= length (head board) || y >= length board) then 0 else
    let nextBoard = set2 pos Nothing board in
    let myNeighbors = map (sumPos pos) neighbors in
    case (board !! y) !! x of
        -- if we've explored this cell, don't worry about it
        Nothing -> 0
        -- if it's a wall, don't worry about it
        Just Wall -> 0
        -- if it's a pill or empty space, explore the neighboring spaces and sum
        Just Pill -> 1 + sum (map (checkBranch nextBoard) myNeighbors) -- jess: seems like a very expensive and not very useful heuristic
        _ -> sum (map (checkBranch nextBoard) myNeighbors)

-- partially navigated board -> current *position* -> (more navigated board, new *offset*, whether to continue)
navigateStep :: Board -> (Int, Int) -> (Board, (Int, Int), Bool)
navigateStep board pos =
    let exBoard = set2 pos Nothing $ toExploration board in
    let neighborPellets = map (\x -> (x, checkBranch exBoard (x `sumPos` pos))) neighbors in
    let bestOffset = fst $ minimumOn snd (filter (\(_, pellets) -> pellets > 0) neighborPellets) in
    let anyLeft = snd (maximumOn snd neighborPellets) > 0 in
    let nextBoard = set2 (pos `sumPos` bestOffset) Empty board in
        (nextBoard, bestOffset, anyLeft)

-- using a tuple because it's easier for strToBoard
navigate :: (Board, (Int, Int)) -> [(Int, Int)]
navigate (board, pos) =
    let (nextBoard, offset, continue) = navigateStep board pos in
    if not continue then [] -- jess: no offset for the last move?
    else offset : navigate (nextBoard, pos `sumPos` offset)

offsetToChar :: (Int, Int) -> Char
offsetToChar (1, 0) = 'R'
offsetToChar (-1, 0) = 'L'
offsetToChar (0, 1) = 'D'
offsetToChar (0, -1) = 'U'
offsetToChar e = error $ "Invalid offset '" ++ show e ++ "'"

-- jess: testing this on lambdaman3, it's extremely slow and ends up going back and forth LRLRLRL etc.
--       my suggestion: greedy BFS
--       pros: - fast
--             - simple
--             - wall-aware
--             - guaranteed optimal path between any two points
--             - pill search and pathing rolled into one
--       cons: - overall path not guaranteed to be optimal (but it's TSP, so nothing will give you that)
--             - might involve a lot of backtracking
solve :: String -> String
solve x = map offsetToChar $ navigate $ strToBoard x

lambdaman4 :: String
lambdaman4 = "......\n.#....\n..#...\n...#..\n..#L#.\n.#...#\n......"

solveLambdaman :: Int -> IO ()
solveLambdaman n = do
  let msg = "get lambdaman" ++ show n
  res <- WebRepl.sendMessage msg
  let solution = solve res
  res2 <- WebRepl.sendMessage $ "solve lambdaman" ++ show n ++ " " ++ solution
  print res2
