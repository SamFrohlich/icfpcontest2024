{-# LANGUAGE LambdaCase #-}
module Spaceships where

import Data.List.Extra (minimumOn, replace, dropEnd, takeEnd)
import Data.List (delete, intersect)
import Flow
import qualified WebRepl


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

-- accs :: Int -> [Acc]
-- accs d
--   | d == 0    = []
--   | d >  0    = Inc : replicate (d - 1) NOOP ++ [Dec]
--   | otherwise = Dec : replicate (abs d - 1) NOOP ++ [Inc]
accs :: Int -> [Acc]
accs = accs2

-- accs & helper functions
inv :: Acc -> Acc
inv Inc = Dec
inv Dec = Inc
inv NOOP = NOOP

insertNoop :: Int -> [Acc] -> [Acc]
insertNoop i xs = take i xs ++ [NOOP] ++ takeEnd (length xs - i) xs

triangulate :: Int -> [Acc]
triangulate i = replicate i Inc ++ replicate i Dec

accs2 :: Int -> [Acc]
accs2 d
  | d == 0 = []
  | d < 0  = map inv $ accs2 (-d)
  | otherwise =
    let n :: Int = floor $ sqrt $ fromIntegral d in
    let triangle = triangulate n in
    let remainder = d - (n*n) in
        if remainder == 0 then triangle
        else if remainder <= n then insertNoop remainder triangle
        else insertNoop (remainder - n) (insertNoop n triangle)

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

toPoints :: String -> String
toPoints s = "[(" ++ (dropEnd 2 (replace " " ", " $ replace "\n" "),(" s)) ++ "]"

getPoints :: Int -> IO String
getPoints n = do
    u <- WebRepl.sendMessage ("get spaceship" ++ show n)
    pure $ toPoints u