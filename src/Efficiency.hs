module Efficiency where
import Hacking (strToAST)
import AST
import Flow
import Data.Function (fix)

toHask :: String -> IO ()
toHask = strToAST .> printAST

-- toHask "B$ L! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! B$ v! I\" L! B+ B+ v! v! B+ v! v!"
e1, e2, e3, e4, e5, e6, e12, e13 :: Int

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

-- toHask "B$ L' B$ L( B$ B$ L\" B$ L# B$ v\" B$ v# v# L# B$ v\" B$ v# v# L$ L% ? B& B> v% I? B$ v' B$ v( v% v% B$ v$ B+ v% I\" I# B$ L\" B$ L# B$ v\" B$ v# v# L# B$ v\" B$ v# v# L$ L% ? B< v% I# I\" B+ B$ v$ B- v% I\" B$ v$ B- v% I# L& B$ B$ L\" B$ L# B$ v\" B$ v# v# L# B$ v\" B$ v# v# L$ L% ? B= v% v& T ? B= B% v& v% I! F B$ v$ B+ v% I\" I#"
e6 = filter 2
  where
    filter x_4 =
      if (x_4 > 30) && (isPrime (fib x_4) 2)
      then x_4
      else filter ((x_4) + (1))
    fib = (\x_4 ->
            if x_4 < 2
            then 1
            else (fib ((x_4) - (1))) + ((fib) ((x_4) - (2))))
    isPrime x_5 x_4 =
                if (x_4) == (x_5)
                then True
                else if ((x_5) `rem` (x_4)) == (0)
                     then False
                     else isPrime x_5 ((x_4) + (1))

-- toHask "B$ B$ L\" B$ L# B$ v\" B$ v# v# L# B$ v\" B$ v# v# L$ L% ? B= v% S I! B+ I\" B$ v$ BD I\" v% B$ L# B. B$ v# B$ v# B$ v# B$ v# B$ v# B$ v# B$ v# B$ v# B$ v# B$ v# B$ v# B$ v# B$ v# B$ v# B$ v# B$ v# B$ v# B$ v# B$ v# B$ v# B$ v# B$ v# B$ v# B$ v# B$ v# B$ v# B$ v# B$ v# S.! S(%9*5$% L\" B. v\" v\""
e13 = f (concat (replicate (2^28) "na") ++ "heyjude")
  where
    f x_4 =
      if (x_4) == ("")
      then 0
      else 1 + f (drop 1 (x_4))
    dup s = s ++ s

-- toHask "B$ B$ L\" B$ L# B$ v\" B$ v# v# L# B$ v\" B$ v# v# L$ L% B$ B$ L\" L# ? B< v\" v# v\" v# v% B+ I\" ? B> v% I# B$ B$ B$ L\" B$ L# B$ v\" B$ v# v# L# B$ v\" B$ v# v# L& L' L( ? B= v' v% v( B$ B$ v& B+ v' I\" ? B> B$ v$ v' B- v' I\" ? B= B% v% v' I! B* B/ v( B$ v$ v' B- B$ v$ v' I\" v( v( I# v% v% I\"Ndb"
e12 = m
  where
    m = 1 + g 2 1234567
    g a b =
      if a == 1234567
      then b
      else g (a + 1) (if a `elem` [1, 127, 9721] -- hypothesis, always true && a <= m
                       then (b `quot` a) * (a - 1)
                       else b)

a `divides` b = b `rem` a == 0
