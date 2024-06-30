module NumberMap where

import Data.Char

type Base94 = String

-- >>> toBase94 1337
-- "/6"
toBase94 :: Integer -> Base94
toBase94 x
  = let q = x `div` 94
        r = x `mod` 94
        c = [chr (fromInteger r + 33)]
    in if q == 0 then c
        else toBase94 q ++ c

-- >>> fromBase94 "/6"
-- 1337
fromBase94 :: Base94 -> Integer
fromBase94 = foldl (\x c -> (94*x) + (fromIntegral (ord c) - 33)) 0
