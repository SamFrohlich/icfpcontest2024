module Spaceships where

import Data.List.Extra (minimumOn)
import Data.List (delete)

nearestNeighbor :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
nearestNeighbor [] curPos = [curPos]
nearestNeighbor coords curPos =
    let first = minimumOn (dist curPos) coords in
        curPos : nearestNeighbor (delete first coords) first

dist :: (Int, Int) -> (Int, Int) -> Int
dist (sx, sy) (tx, ty) = (tx - sx) + (ty - sy)