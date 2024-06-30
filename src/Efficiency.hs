module Efficiency where
import Hacking (strToAST)
import AST
import Flow
import Data.Function (fix)

toHask :: String -> IO ()
toHask = strToAST .> printAST

-- toHask "B$ L! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! I\" L! B+ B+ v! v! B+ v! v!"
e1, e2, e3, e4, e5 :: Integer
-- 17592186044416
e1 = (\x_0 -> (x_0) ((x_0) ((x_0) ((x_0) ((x_0) ((x_0) ((x_0) ((x_0) ((x_0) ((x_0) ((x_0) ((x_0) ((x_0) ((x_0) ((x_0) ((x_0) ((x_0) ((x_0) ((x_0) ((x_0) ((x_0) ((x_0) (1))))))))))))))))))))))) (\x_0 -> ((x_0) + (x_0)) + ((x_0) + (x_0)))

-- toHask "B+ I7c B* B$ B$ L\" B$ L# B$ v\" B$ v# v# L# B$ v\" B$ v# v# L$ L% ? B= v% I! I\" B+ I\" B$ v$ B- v% I\" I\":c1+0 I!"
-- 2134
e2 = (2134) + ((fix f (9345873499 :: Integer)) * (0))
  where
    f = \recur -> \x -> if x == 0 then 1 else 1 + recur (x - 1)

-- toHask "B+ I7c B* B$ B$ L\" B$ L# B$ v\" B$ v# v# L# B$ v\" B$ v# v# L$ L% ? B= v% I! I\" B+ I\" B$ v$ B- v% I\" I\":c1+0 I\""
-- 9345875634
e3 = (2134) + ((fix f (9345873499 :: Integer)) * 1)
  where
    f = \recur -> \x -> if x == 0 then 1 else 1 + recur (x - 1)

-- toHask "B$ B$ L\" B$ L# B$ v\" B$ v# v# L# B$ v\" B$ v# v# L$ L% ? B< v% I# I\" B+ B$ v$ B- v% I\" B$ v$ B- v% I# II"
-- it's fibonacci (shifted by 1)
-- fib 41 = 165580141
e4 = (fix f) (40)
  where
    f :: (Integer -> Integer) -> Integer -> Integer
    f recur x = if x < 2 then 1 else recur (x - 1) + recur (x - 2)

-- toHask "B$ L' B$ L( B$ B$ L\" B$ L# B$ v\" B$ v# v# L# B$ v\" B$ v# v# L$ L% ? B& B> v% I\"41= B& B$ v' v% B$ v( B+ v% I\" v% B$ v$ B+ v% I\" I# B$ L\" B$ L# B$ v\" B$ v# v# L# B$ v\" B$ v# v# L$ L% ? B= v% I\" T ? B= B% v% I# I\" F B$ v$ B/ v% I# L& B$ B$ L\" B$ L# B$ v\" B$ v# v# L# B$ v\" B$ v# v# L$ L% ? B= v% v& T ? B= B% v& v% I! F B$ v$ B+ v% I\" I#"
e5 = findMersennePrime 2
  where
    -- find the first Mersenne prime (2^n - 1) above 1000000
    findMersennePrime x
      |    x > 1000000
        && isPrime x 2
        && isPowerOf2 (x + 1) = x
      | otherwise = findMersennePrime (x + 1)

    -- runs until it finds the first prime factor of x (will always return true eventually)
    -- is x a prime number?
    isPrime x y
      | y == x = True
      | (x `rem` y) == 0 = False
      | otherwise = isPrime x (y + 1)

    -- is power of 2
    isPowerOf2 x
      | x == 1 = True
      | (x `rem` 2) == 1 = False
      | otherwise = isPowerOf2 (x `quot` 2)

    -- f recur x
    --   |    x > 1000000
    --     && hRec x 2
    --     && gRec (x + 1) = x
    --   | otherwise = recur (x + 1)



    -- g recur x
    --   | x == 1 = True
    --   | (x `rem` 2) == 1 = False
    --   | otherwise = recur (x `div` 2)



    -- h x recur y
    --   | y == x = True
    --   | (x `rem` y) == 0 = False
    --   | otherwise = recur (y + 1)