{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module RLE where

import Prelude (String, Integer, Bool, ($), undefined, IO, length, fromIntegral)
import HOAS
import Eval (int2string)
import qualified Prelude as P
import Data.List (group, concatMap)
import Flow

------------------------------------------------
-- Run-length encoding/decoding
------------------------------------------------

rleString :: String -> HOASe String
rleString str = decode $~ (s $ encode str)

-- encode :: [Move] -> [Either Move (Int, Move)]
encode :: String -> String
encode
  = group
 .> concatMap (\ms@(m:_) -> m : int2string (fromIntegral $ length ms))
 .> (P.++ "E") -- needs a terminator for the string

decode :: HOASe (String -> String)
decode = fix $~ f
  where
    f = l $ \recur -> l $ \str ->
          let mv = take (i 1) str in
          if' (mv == (s "E")) -- if end token reached
            (drop (i 1) mv) -- not sure if this will work since no empty string literals
            (let n  = s2i (take (i 1) (drop (i 1) str))
                 mvs = replicate $~ mv $~ n
             in mvs ++ (recur $~ (drop (i 2) str)))

replicate :: HOASe (String -> Integer -> String)
replicate = l $ \str -> 
  fix $~ (l $ \recur -> l $ \n ->
            if' (n == i 1)
              str
              (str ++ (recur $~ (n - i 1))))

roundTrip :: String -> IO String
roundTrip str = evalHOASe $ decode $~ (s $ encode str)

egRLE :: HOASe String
egRLE = s ("U" P.++ int2string 5 P.++ "E")

egRLEeval :: IO String
egRLEeval = evalHOASe $ decode $~ egRLE
