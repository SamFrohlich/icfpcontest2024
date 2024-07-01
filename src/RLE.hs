{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module RLE where

import Prelude (String, Integer, Bool, ($), undefined, IO, length, fromIntegral, Int, otherwise)
import HOAS
import Eval (int2string)
import qualified Prelude as P
import Data.List (group, concatMap)
import Flow
import NumberMap (toBase94)
import StringMap (fromIcfpChar, fromIcfpString)

------------------------------------------------
-- Run-length encoding/decoding
------------------------------------------------

-- n digit RLE
rleString :: Integer -> String -> HOASe String
rleString digits str = decode digits $~ (s $ encode digits str)

-- n digit encode
encode :: Integer -> String -> String
encode digits
  = group
 .> concatMap (\ms@(m:_) -> m : pad digits (int2string (fromIntegral $ length ms)))
 .> (P.++ "E") -- needs a terminator for the string

pad :: Integer -> String -> String
pad d ns
  | d P.< fromIntegral (length ns) = P.error "RLE number is out of bounds!"
  | otherwise = P.concat (P.replicate (fromIntegral d P.- length ns) (int2string 0)) P.++ ns

-- n-digit RLE
decode :: Integer -> HOASe (String -> String)
decode digits = fix $~ f
  where
    f = l $ \recur -> l $ \str ->
          let mv = take (i 1) str in
          if' (mv == (s "E")) -- if end token reached
            (drop (i 1) mv)
            (let n  = s2i (take (i digits) (drop (i 1) str))
                 mvs = replicate $~ mv $~ n
             in mvs ++ (recur $~ (drop (i (1 P.+ digits)) str)))

replicate :: HOASe (String -> Integer -> String)
replicate = l $ \str -> 
  fix $~ (l $ \recur -> l $ \n ->
            if' (n == i 1)
              str
              (str ++ (recur $~ (n - i 1))))

-- roundTrip :: String -> IO String
-- roundTrip str = evalHOASe $ decode $~ (s $ encode str)

egRLE :: HOASe String
egRLE = s ("U" P.++ int2string 5 P.++ "E")

egRLEeval :: IO String
egRLEeval = evalHOASe $ decode 1 $~ egRLE
