{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module HOAS where

import Prelude (Bool, Integer, String, IO, ($))
import Prelude qualified as P

import AST qualified
import Data.Map.Strict (Map)
import Data.Dynamic (Dynamic, Typeable)
import Eval (evalTo)
import Flow

------------------------------------------------------
-- HOAS
------------------------------------------------------
class HOAS exp where
  l :: (exp a -> exp b) -> exp (a -> b)
  ($~) :: exp (a -> b) -> exp a -> exp b
  v :: Integer -> exp a

  if' :: exp Bool -> exp a -> exp a -> exp a

  -- literals
  true :: exp Bool
  false :: exp Bool
  i :: Integer -> exp Integer
  s :: String -> exp String
  
  -- unary functions
  neg :: exp Integer -> exp Integer
  not :: exp Bool -> exp Bool
  s2i :: exp String -> exp Integer
  i2s :: exp Integer -> exp String

  -- binary operators
  (+)  :: exp Integer -> exp Integer -> exp Integer
  (-)  :: exp Integer -> exp Integer -> exp Integer
  (*)  :: exp Integer -> exp Integer -> exp Integer
  (/)  :: exp Integer -> exp Integer -> exp Integer
  (%)  :: exp Integer -> exp Integer -> exp Integer
  (<)  :: exp Integer -> exp Integer -> exp Bool
  (>)  :: exp Integer -> exp Integer -> exp Bool
  (||) :: exp Bool    -> exp Bool    -> exp Bool
  (&&) :: exp Bool    -> exp Bool    -> exp Bool
  (++) :: exp String  -> exp String  -> exp String

  -- binary functions
  take :: exp Integer -> exp String -> exp String
  drop :: exp Integer -> exp String -> exp String

  -- recursion
  fix :: exp ((a -> a) -> a)

class HOAS exp => EQHOAS exp a where
  (==) :: exp a    -> exp a    -> exp Bool

instance EQHOAS HOASe Bool where
  (==) = EQB

instance EQHOAS HOASe String where
  (==) = EQS

instance EQHOAS HOASe Integer where
  (==) = EQI


data HOASe a where
  L :: (HOASe a -> HOASe b) -> HOASe (a -> b)
  (:$) :: HOASe (a -> b) -> HOASe a -> HOASe b
  V :: Integer -> HOASe a

  If :: HOASe Bool -> HOASe a -> HOASe a -> HOASe a

  -- literals
  T :: HOASe Bool
  F :: HOASe Bool
  I :: Integer -> HOASe Integer
  S :: String -> HOASe String
  
  -- unary functions
  Neg :: HOASe Integer -> HOASe Integer
  Not :: HOASe Bool -> HOASe Bool
  S2I :: HOASe String -> HOASe Integer
  I2S :: HOASe Integer -> HOASe String

  -- binary operators
  Add :: HOASe Integer -> HOASe Integer -> HOASe Integer
  Sub :: HOASe Integer -> HOASe Integer -> HOASe Integer
  Mul :: HOASe Integer -> HOASe Integer -> HOASe Integer
  Div :: HOASe Integer -> HOASe Integer -> HOASe Integer
  Mod :: HOASe Integer -> HOASe Integer -> HOASe Integer
  LT  :: HOASe Integer -> HOASe Integer -> HOASe Bool
  GT  :: HOASe Integer -> HOASe Integer -> HOASe Bool
  EQI :: HOASe Integer -> HOASe Integer -> HOASe Bool
  EQS :: HOASe String  -> HOASe String  -> HOASe Bool
  EQB :: HOASe Bool    -> HOASe Bool    -> HOASe Bool
  Or  :: HOASe Bool    -> HOASe Bool    -> HOASe Bool
  And :: HOASe Bool    -> HOASe Bool    -> HOASe Bool
  Cat :: HOASe String  -> HOASe String  -> HOASe String 

  -- binary functions
  Take :: HOASe Integer -> HOASe String -> HOASe String
  Drop :: HOASe Integer -> HOASe String -> HOASe String

  -- recursion
  Fix :: HOASe ((a -> a) -> a)



instance HOAS HOASe where
  l = L
  ($~) = (:$)
  v = V

  if' = If

  -- literals
  true = T
  false = F
  i = I
  s = S
  
  -- unary functions
  neg = Neg
  not = Not
  s2i = S2I
  i2s = I2S

  -- binary operators
  (+)  = Add
  (-)  = Sub
  (*)  = Mul
  (/)  = Div
  (%)  = Mod
  (<)  = LT  
  (>)  = GT  
  -- (==) = EQ  
  (||) = Or  
  (&&) = And 
  (++) = Cat 

  -- binary functions
  take = Take
  drop = Drop

  -- recursion
  fix = Fix


hoasToAST :: HOASe a -> AST.ICFP Integer
hoasToAST = go 1
  where
    go :: Integer -> HOASe a -> AST.ICFP Integer
    go nextIdent = \case
      L g -> AST.L nextIdent (go (nextIdent P.+ 1) $ g (V nextIdent))
      g :$ x -> AST.B AST.App (go nextIdent g) (go nextIdent x)
      V n -> AST.V n

      If b x y -> AST.If (go nextIdent b) (go nextIdent x) (go nextIdent y)

      T     -> AST.T
      F     -> AST.F
      I x   -> AST.I x
      S str -> AST.S str

      Neg x -> AST.U AST.Neg (go nextIdent x)
      Not x -> AST.U AST.Not (go nextIdent x)
      S2I x -> AST.U AST.S2I (go nextIdent x)
      I2S x -> AST.U AST.I2S (go nextIdent x)

      Add x y -> AST.B AST.Add (go nextIdent x) (go nextIdent y)
      Sub x y -> AST.B AST.Sub (go nextIdent x) (go nextIdent y)
      Mul x y -> AST.B AST.Mul (go nextIdent x) (go nextIdent y)
      Div x y -> AST.B AST.Div (go nextIdent x) (go nextIdent y)
      Mod x y -> AST.B AST.Mod (go nextIdent x) (go nextIdent y)
      LT  x y -> AST.B AST.LT  (go nextIdent x) (go nextIdent y)
      GT  x y -> AST.B AST.GT  (go nextIdent x) (go nextIdent y)
      EQB x y -> AST.B AST.EQ  (go nextIdent x) (go nextIdent y)
      EQS x y -> AST.B AST.EQ  (go nextIdent x) (go nextIdent y)
      EQI x y -> AST.B AST.EQ  (go nextIdent x) (go nextIdent y)
      Or  x y -> AST.B AST.Or  (go nextIdent x) (go nextIdent y)
      And x y -> AST.B AST.And (go nextIdent x) (go nextIdent y)
      Cat x y -> AST.B AST.Cat (go nextIdent x) (go nextIdent y)

      Take n str -> AST.B AST.Take  (go nextIdent n) (go nextIdent str)
      Drop n str -> AST.B AST.Drop  (go nextIdent n) (go nextIdent str)

      Fix -> AST.L false' (AST.B AST.App
                         (AST.L x' (AST.B AST.App (AST.V false') (AST.B AST.App (AST.V x') (AST.V x'))))
                         (AST.L x' (AST.B AST.App (AST.V false') (AST.B AST.App (AST.V x') (AST.V x')))))
        where
          false' = nextIdent
          x' = nextIdent P.+ 1
      -- _ -> _

evalHOASe :: Typeable a => HOASe a -> IO a
evalHOASe = hoasToAST .> evalTo

eg :: HOASe Bool
eg = (L (\b -> b)) :$ T

egHOASICFP :: AST.ICFP Integer
egHOASICFP = hoasToAST eg

eg2 :: HOASe (b -> b)
eg2 = (L (\b -> b)) :$ (L (\b -> b))

eg2HOASICFP :: AST.ICFP Integer
eg2HOASICFP = hoasToAST eg2

eg3 :: IO Bool
eg3 = evalHOASe $ l (\b -> b) $~ true

eg4 :: IO Integer
eg4 = evalHOASe $ fix $~ (l $ \recur -> l $ \x -> if' (x == i 1) x (x * (recur $~ (x - i 1)))) $~ i 4
