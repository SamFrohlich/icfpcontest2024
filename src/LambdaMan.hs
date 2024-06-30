{-# LANGUAGE RecordWildCards #-}
module LambdaMan where

import Data.List.Extra (takeEnd, minimumOn, maximumOn, splitOn, elemIndex, findIndex)
import Data.Maybe (isJust, fromJust)
import qualified WebRepl
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Sequence (Seq((:<|)))
import Data.Sequence qualified as Seq

import Flow
import Data.Set (Set)
import Data.Set qualified as S 
import Data.List (find, group)
import Data.Function (fix)
import Debug.Trace (traceShow, trace)

solveLambdaman :: Int -> IO ()
solveLambdaman n = do
  let msg = "get lambdaman" ++ show n
  res <- WebRepl.sendMessage msg
  let solution = solveLambdamanPure res
  print solution
  res2 <- WebRepl.sendMessage $ "solve lambdaman" ++ show n ++ " " ++ solution
  print res2

solveLambdamanPure :: String -> String
solveLambdamanPure = strToBoard .> lambdaman .> movesToStr

movesToStr :: [Move] -> String
movesToStr = map show .> concat

-- solveFromFile :: FilePath -> IO String
solveFromFile file = do
  puzzle <- readFile file
  pure $ (strToBoard .> lambdaman) puzzle

------------------------------------------------
-- Refactor
------------------------------------------------

data Board = Board
  { grid :: Map (Int, Int) Cell
  , xMax :: Int
  , yMax :: Int
  } deriving (Show, Eq)

data Cell
    = Wall
    | Start
    | Empty
    | Pill
    deriving (Eq, Ord, Show)

index :: Board -> Int -> Int -> Cell
index Board{..} x y
  | x > xMax = Wall
  | y > yMax = Wall
  | otherwise = case grid M.!? (x, y) of
      Just c -> c
      _      -> Wall

insert :: (Int, Int) -> Cell -> Board -> Board
insert pos c Board{..} = Board{grid = M.insert pos c grid, xMax = xMax, yMax = yMax}

-- to1D :: Int -> Int -> Int -> Int
-- to1D xMax x y = (xMax + 1) * y + x

strToBoard :: String -> Board
strToBoard s = Board{..}
  where
    gridLists :: [[Cell]]
    gridLists
      = s |> splitOn "\n"
          |> filter (not . null)
          |> map (map charToCell)
    
    xMax = maximum (map length gridLists)
    yMax = length gridLists

    -- grid :: IntMap Cell
    gridCoords :: [(Int, [(Int, Cell)])]
    gridCoords
      = gridLists
        |> map (zip [0..])
        |> zip [0..]
    
    grid = M.fromList
      [ ((x,y), c)
      | (y, xs) <- gridCoords
      , (x, c)  <- xs
      ]

charToCell :: Char -> Cell
charToCell 'L' = Start
charToCell '#' = Wall
charToCell '.' = Pill
charToCell ' ' = Empty
charToCell c = error $ "Invalid character '" ++ [c] ++ "' in Lambdaman board"

lambdaman1 :: String
lambdaman1 = "###.#...\n...L..##\n.#######"

data Move = U | R | D | L deriving (Show, Eq, Ord)

-- maybe reversed path, in terms of Moves from a starting point
-- Nothing if unvisited
-- initBFSBoard :: Board -> Board (Maybe [Move])
-- initBFSBoard Board{..} = Board
--   { grid = fmap (const Nothing) grid
--   , xMax = xMax
--   , yMax = yMax
--   }

type FIFO = Seq ((Int, Int), [Move])

-- add :: (Int, Int) -> [Move] -> FIFO -> FIFO
-- add pos path = (Seq.|> (pos, path))


lambdaman :: Board -> [Move]
lambdaman initBoard = go initBoard (findStart initBoard) []
  where
    go board pos moves = case newMoves of
      [] -> reverse moves -- reverse?
      _  -> go board' pos' (newMoves ++ moves)
      where
        (pos', newMoves) = bfs board pos
        board' = insert pos' Empty board

findStart :: Board -> (Int, Int)
findStart
  = grid
    .> M.toList
    .> find (\(pos, c) -> c == Start)
    .> fromJust -- could error, but shouldn't
    .> fst

bfs :: Board -> (Int, Int) -> ((Int, Int), [Move])
bfs board pos = case go (Seq.singleton (pos, [])) (S.singleton pos) of
  Left Nothing -> (pos, [])
  Left (Just res) -> res
  Right _ -> error "step should always eventually return a Left"
  where
    go queue visited = do
      (queue', visited') <- step board queue visited
      go queue' visited'

step :: Board -> FIFO -> Set (Int, Int) -> Either (Maybe ((Int, Int), [Move])) (FIFO, Set (Int, Int))
step board queue visited = case queue of
  ((x, y), rpath) :<| queue'
    -> case index board x y of
         Pill -> Left $ Just ((x, y), rpath)
         Wall -> Right (queue', visited)
         -- empty or start:
         _    -> Right (queue' Seq.>< neighbourQueue, neighbourSet `S.union` visited)
           where
             neighbourQueue
               = unvisitedNeighbourList
                 |> map (\(pos', mv) -> (pos', mv : rpath))
                 |> Seq.fromList
             neighbourSet 
               = unvisitedNeighbourList
                 |> map fst
                 |> S.fromList

             unvisitedNeighbourList = unvisitedNeighbours visited (x,y)
  Seq.Empty -> Left Nothing -- Nowhere left to go, don't move, end search

unvisitedNeighbours :: Set (Int, Int) -> (Int, Int) -> [((Int, Int), Move)]
unvisitedNeighbours visited pos
  = neighbours pos
    |> filter (\(pos', _) -> not $ pos' `S.member` visited)

neighbours :: (Int, Int) -> [((Int, Int), Move)]
neighbours (x, y) = [ ((x + dx, y + dy), mv) | ((dx, dy), mv) <- neighbourOffsets]

neighbourOffsets :: [((Int, Int), Move)]
neighbourOffsets = [((1, 0), R), ((-1, 0), L), ((0, 1), D), ((0, -1), U)]


------------------------------------------------

lamman21 :: String
lamman21 = (fix (\x_1 -> \x_2 -> if (x_2) == (0) then "........................................................................................................................................................................................................" else ((x_1) ((x_2) `quot` (4))) ++ ((1) `take` (((x_2) `rem` (4)) `drop` (".\n#L"))))) bigNum

bigNum :: Int
bigNum = 2 ^ 20 + 1 

------------------------------------------------
-- Run-length encoding/decoding
------------------------------------------------

-- encode :: [Move] -> [(Int, Move)]


------------------------------------------------
-- Old method
------------------------------------------------

-- build a DFS tree from lambdaman's position
-- take the branch that has the least uneaten pellets

-- neighbors :: [(Int, Int)]
-- neighbors = [(1, 0), (-1, 0), (0, 1), (0, -1)]

-- set :: Int -> a -> [a] -> [a]
-- set i v vs = take i vs ++ [v] ++ takeEnd (length vs - i - 1) vs

-- set2 :: (Int, Int) -> a -> [[a]] -> [[a]]
-- set2 (x, y) v vs = set y (set x v (vs !! y)) vs

-- find2 :: Eq a => a -> [[a]] -> (Int, Int)
-- find2 v vs =
--     let indices = map (elemIndex v) vs in
--     let y = fromJust $ findIndex isJust indices in
--         (fromJust $ indices !! y, y)

-- sumPos :: (Int, Int) -> (Int, Int) -> (Int, Int)
-- sumPos (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- data Cell
--     = Empty
--     | Start
--     | Pill
--     | Wall
--     deriving (Eq, Show)

-- type Board = [[Cell]]

-- -- keep track of whether a cell has been visited
-- type ExplorationBoard = [[Maybe Cell]]

-- toExploration :: Board -> ExplorationBoard
-- toExploration = map (map Just)

-- -- text representation of the board -> (the board, starting position)
-- strToBoard :: String -> (Board, (Int, Int))
-- strToBoard s =
--     let b = map (map charToCell) $ filter (not . null) (splitOn "\n" s) in
--         (b, find2 Start b)

-- charToCell :: Char -> Cell
-- charToCell 'L' = Start
-- charToCell '#' = Wall
-- charToCell '.' = Pill
-- charToCell ' ' = Empty -- for debugging
-- charToCell c = error $ "Invalid character '" ++ [c] ++ "' in Lambdaman board"

-- -- partially explored board -> position -> number of pellets
-- checkBranch :: ExplorationBoard -> (Int, Int) -> Int
-- checkBranch board pos@(x, y) =
--     if (x < 0 || y < 0 || x >= length (head board) || y >= length board) then 0 else
--     let nextBoard = set2 pos Nothing board in
--     let myNeighbors = map (sumPos pos) neighbors in
--     case (board !! y) !! x of
--         -- if we've explored this cell, don't worry about it
--         Nothing -> 0
--         -- if it's a wall, don't worry about it
--         Just Wall -> 0
--         -- if it's a pill or empty space, explore the neighboring spaces and sum
--         Just Pill -> 1 + sum (map (checkBranch nextBoard) myNeighbors) -- jess: seems like a very expensive and not very useful heuristic
--         _ -> sum (map (checkBranch nextBoard) myNeighbors)

-- -- partially navigated board -> current *position* -> (more navigated board, new *offset*, whether to continue)
-- navigateStep :: Board -> (Int, Int) -> (Board, (Int, Int), Bool)
-- navigateStep board pos =
--     let exBoard = set2 pos Nothing $ toExploration board in
--     let neighborPellets = map (\x -> (x, checkBranch exBoard (x `sumPos` pos))) neighbors in
--     let bestOffset = fst $ minimumOn snd (filter (\(_, pellets) -> pellets > 0) neighborPellets) in
--     let anyLeft = snd (maximumOn snd neighborPellets) > 0 in
--     let nextBoard = set2 (pos `sumPos` bestOffset) Empty board in
--         (nextBoard, bestOffset, anyLeft)

-- -- using a tuple because it's easier for strToBoard
-- navigate :: (Board, (Int, Int)) -> [(Int, Int)]
-- navigate (board, pos) =
--     let (nextBoard, offset, continue) = navigateStep board pos in
--     if not continue then [] -- jess: no offset for the last move?
--     else offset : navigate (nextBoard, pos `sumPos` offset)

-- offsetToChar :: (Int, Int) -> Char
-- offsetToChar (1, 0) = 'R'
-- offsetToChar (-1, 0) = 'L'
-- offsetToChar (0, 1) = 'D'
-- offsetToChar (0, -1) = 'U'
-- offsetToChar e = error $ "Invalid offset '" ++ show e ++ "'"

-- -- jess: testing this on lambdaman3, it's extremely slow and ends up going back and forth LRLRLRL etc.
-- --       my suggestion: greedy BFS
-- --       pros: - fast
-- --             - simple
-- --             - wall-aware
-- --             - guaranteed optimal path between any two points
-- --             - pill search and pathing rolled into one
-- --       cons: - overall path not guaranteed to be optimal (but it's TSP, so nothing will give you that)
-- --             - might involve a lot of backtracking
-- solve :: String -> String
-- solve x = map offsetToChar $ navigate $ strToBoard x

-- lambdaman4 :: String
-- lambdaman4 = "......\n.#....\n..#...\n...#..\n..#L#.\n.#...#\n......"

-- solveLambdaman :: Int -> IO ()
-- solveLambdaman n = do
--   let msg = "get lambdaman" ++ show n
--   res <- WebRepl.sendMessage msg
--   let solution = solve res
--   res2 <- WebRepl.sendMessage $ "solve lambdaman" ++ show n ++ " " ++ solution
--   print res2
