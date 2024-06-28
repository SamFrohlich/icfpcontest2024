{-# LANGUAGE LambdaCase #-}
module Spaceships where

import Data.List.Extra (minimumOn)
import Data.List (delete, intersect)
import Flow


nearestNeighbor :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
nearestNeighbor curPos [] = [curPos]
nearestNeighbor curPos coords =
    let first = minimumOn (dist curPos) coords in
        curPos : nearestNeighbor first (delete first coords) 

dist :: (Int, Int) -> (Int, Int) -> Int
dist (sx, sy) (tx, ty) = (tx - sx) + (ty - sy)


newtype CMD = CMD (Acc, Acc)
  deriving (Eq, Ord, Show)

data Acc
  = Inc
  | Dec
  | NOOP
  deriving (Eq, Ord, Show)

pointToPoint :: (Int, Int) -> (Int, Int) -> [CMD]
pointToPoint (x0, y0) (x1, y1)
  = zipDefault NOOP xAccs yAccs
    |> map CMD
  where
    xAccs = accs (x1 - x0)
    yAccs = accs (y1 - y0)

accs :: Int -> [Acc]
accs d
  | d == 0    = []
  | d >  0    = Inc : replicate (d - 1) NOOP ++ [Dec]
  | otherwise = Dec : replicate (abs d - 1) NOOP ++ [Inc]

zipDefault :: a -> [a] -> [a] -> [(a, a)]
zipDefault _ []     []     = []
zipDefault d []     (y:ys) = (d, y) : zipDefault d [] ys
zipDefault d (x:xs) []     = (x, d) : zipDefault d xs []
zipDefault d (x:xs) (y:ys) = (x, y) : zipDefault d xs ys

pathToCMDs :: [(Int, Int)] -> [CMD]
pathToCMDs []   = []
pathToCMDs [_] = [] 
pathToCMDs (p1 : p2 : ps) = pointToPoint p1 p2 ++ pathToCMDs (p2 : ps)

pointsToCMDs :: [(Int, Int)] -> [CMD]
pointsToCMDs = nearestNeighbor (0,0) .> pathToCMDs

egPoints :: [(Int, Int)]
egPoints = [(1,-1), (1, -3), (2,-5), (2, -8), (3, -10)]

-- >>> pointsToCMDs egPoints
-- [CMD (Inc,Dec),CMD (NOOP,NOOP),CMD (NOOP,NOOP),CMD (Dec,NOOP),CMD (NOOP,NOOP),CMD (NOOP,NOOP),CMD (NOOP,NOOP),CMD (NOOP,NOOP),CMD (NOOP,NOOP),CMD (NOOP,NOOP),CMD (NOOP,Inc),CMD (Dec,Inc),CMD (Inc,NOOP),CMD (NOOP,Dec),CMD (NOOP,Inc),CMD (NOOP,NOOP),CMD (NOOP,NOOP),CMD (NOOP,Dec),CMD (Dec,Inc),CMD (Inc,NOOP),CMD (NOOP,Dec),CMD (NOOP,Inc),CMD (NOOP,NOOP),CMD (NOOP,Dec)]

eval :: [CMD] -> [(Int, Int)]
eval = go (0, 0) (0,0)
  where
    go (x,y) (dx,dy) [] = [(x,y)]
    go (x,y) (dx,dy) (CMD (ddx, ddy):cmds)
      = (x,y) : go (x', y') (dx', dy') cmds
      where
        dx' = dx + toVel ddx
        dy' = dy + toVel ddy

        x' = x + dx'
        y' = y + dy'

toVel :: Acc -> Int
toVel = \case
  Inc -> 1
  Dec -> -1
  NOOP -> 0

allIn :: Eq a => [a] -> [a] -> Bool
allIn xs ys = length (xs `intersect` ys) == length xs

visitsAllPoints :: [(Int, Int)] -> Bool
visitsAllPoints points
  = points `allIn` eval (pointsToCMDs points)

toNumpadChar :: CMD -> Char
toNumpadChar (CMD (Dec, Dec)) = '1'
toNumpadChar (CMD (NOOP, Dec)) = '2'
toNumpadChar (CMD (Inc, Dec)) = '3'
toNumpadChar (CMD (Dec, NOOP)) = '4'
toNumpadChar (CMD (NOOP, NOOP)) = '5'
toNumpadChar (CMD (Inc, NOOP)) = '6'
toNumpadChar (CMD (Dec, Inc)) = '7'
toNumpadChar (CMD (NOOP, Inc)) = '8'
toNumpadChar (CMD (Inc, Inc)) = '9'

toNumpadString :: [CMD] -> String
toNumpadString = map toNumpadChar
