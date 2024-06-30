{-# LANGUAGE GADTs, TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Eval where

import AST
import NumberMap
import StringMap

import Prelude hiding (Ordering(..))

import Control.Monad.Trans.State.Strict (StateT, get, evalStateT, modify)

import Data.Map (Map)
import Data.Map qualified as M

import Data.Dynamic (Dynamic (Dynamic), toDyn, fromDyn, Typeable, dynTypeRep, dynApp, fromDynamic)
import Data.Unique
import Control.Monad.IO.Class (liftIO, MonadIO)

import Hacking (strToAST)
import Data.Function (fix)
import Data.Data (Proxy (..))
import Data.Maybe (fromMaybe)
import Control.Applicative (Alternative((<|>)))

import Debug.Trace
import Flow

-- You pretty much always want to use this function,
-- using a type annotation to specify what type you expect the term to evaluate to,
-- e.g.`evalStr @String "B$ B$ L# L$ v# B. SB%,,/ S}Q/2,$_ IK"`
-- which will evaluate to "Hello World!"
evalStr :: Typeable a => String -> IO a
evalStr str = fromDynUnsafe <$> eval (strToAST str)

-- e.g.`evalTo @Bool (B App (L 1 (V 1)) T)`
-- which will evaluate to `True`
-- NOTE: eval and evalTo only evaluates closed terms
evalTo :: Typeable a => ICFP Var -> IO a
evalTo term = fromDynUnsafe <$> eval term

lambdaEg :: String
lambdaEg = "B$ B$ L# L$ v# B. SB%,,/ S}Q/2,$_ IK" -- evals to String

-- >>> strToAST lambdaEg
-- B App (B App (L 2 (L 3 (V 2))) (B Cat (S "Hello") (S " World!"))) (I 42)

limitsEg :: String -- evals to Integer
limitsEg = "B$ B$ L\" B$ L# B$ v\" B$ v# v# L# B$ v\" B$ v# v# L\" L# ? B= v# I! I\" B$ L$ B+ B$ v\" v$ B$ v\" v$ B- v# I\" I%"

-- limitsUntypdLam
  -- = ((\b -> (\c -> b (c c)) (\c -> b (c c))) (\b -> \c -> if (c == 0) then 1 else (\d -> (b d) + (b d)) (c - 1))) 4
  -- = Y (\b -> \c -> if (c == 0) then 1 else (\d -> (b d) + (b d)) (c - 1)) 4

limitsHaskell :: Integer
limitsHaskell = fix f 4
  where
    f :: (Integer -> Integer) -> Integer -> Integer
    f recur x = if x == 0
                then 1
                else (\y -> (recur y) + (recur y)) (x - 1)

-- limitsHaskell
--   ==> fix f 4
--   ==> f (fix f) 4
--   ==> if 4 < 0
        -- then 1
        -- else (\y -> (fix f y) + (fix f y)) (4 - 1)
--   ==> (\y -> (fix f y) + (fix f y)) (4 - 1)
--   ==> (fix f (4 - 1)) + (fix f (4 - 1))
--   ==> (fix f (4 - 1)) + (fix f (4 - 1))
--   ==> (f (fix f) (4 - 1)) + (f (fix f) (4 - 1))
--   ==> ((\y -> (fix f y) + (fix f y)) (3 - 1)) + ((\y -> (fix f y) + (fix f y)) (3 - 1))
--   ==> ...
--   ==> (((fix f 1) + (fix f 1)) + ((fix f 1) + (fix f 1))) + (((fix f 1) + (fix f 1)) + ((fix f 1) + (fix f 1)))
--   ==> ((((fix f 0) + (fix f 0)) + ((fix f 0) + (fix f 0))) + (((fix f 0) + (fix f 0)) + ((fix f 0) + (fix f 0)))) + ((((fix f 0) + (fix f 0)) + ((fix f 0) + (fix f 0))) + (((fix f 0) + (fix f 0)) + ((fix f 0) + (fix f 0))))

limitsEgSmaller :: String -- evals to Integer
limitsEgSmaller = "B$ B$ L\" B$ L# B$ v\" B$ v# v# L# B$ v\" B$ v# v# L\" L# ? B= v# I! I\" B$ L$ B+ B$ v\" v$ B$ v\" v$ B- v# I\" I\""


-- The IO here is for alpha conversion, using GHC's built in
-- `newUnique :: IO Unique` to guarantee unique symbols
-- NOTE: there's no handling of errors atm because I couldn't be bothered
--       and there shouldn't be any unless there's a bug in my implementation or
--       they give us ill-formed or open terms 
eval :: ICFP Var -> IO Dynamic
eval term = do
  term' <- alphaConvert term
  eval' M.empty term'
  where
    eval' :: Map Unique (ICFP Unique) -> ICFP Unique -> IO Dynamic
    eval' env curTerm = case curTerm of
      -- T          -> traceShowICFP env curTerm $ toDyn True
      -- F          -> traceShowICFP env curTerm $ toDyn False
      -- I z        -> traceShowICFP env curTerm $ toDyn z
      -- S s        -> traceShowICFP env curTerm $ toDyn s
      -- U Neg z    -> traceShowICFP env curTerm $ liftDyn @Integer negate (eval' env z)
      -- U Not b    -> traceShowICFP env curTerm $ liftDyn @Bool not (eval' env b)
      -- U S2I s    -> traceShowICFP env curTerm $ liftDyn fromBase94 (eval' env s)
      -- U I2S z    -> traceShowICFP env curTerm $ liftDyn toBase94 (eval' env z)
      -- B Add x y  -> traceShowICFP env curTerm $ liftDyn2 @Integer (+) (eval' env x) (eval' env y)
      -- B Sub x y  -> traceShowICFP env curTerm $ liftDyn2 @Integer (-) (eval' env x) (eval' env y)
      -- B Mul x y  -> traceShowICFP env curTerm $ liftDyn2 @Integer (*) (eval' env x) (eval' env y)
      -- B Div x y  -> traceShowICFP env curTerm $ liftDyn2 @Integer div (eval' env x) (eval' env y)
      -- B Mod x y  -> traceShowICFP env curTerm $ liftDyn2 @Integer mod (eval' env x) (eval' env y)
      -- B LT x y   -> traceShowICFP env curTerm $ liftDyn2 @Integer (<) (eval' env x) (eval' env y) -- NOTE assuming strict
      -- B GT x y   -> traceShowICFP env curTerm $ liftDyn2 @Integer (>) (eval' env x) (eval' env y)
      -- B EQ x y   -> traceShowICFP env curTerm $ dynEq (eval' env x) (eval' env y)
      -- B Or x y   -> traceShowICFP env curTerm $ liftDyn2 (||) (eval' env x) (eval' env y)
      -- B And x y  -> traceShowICFP env curTerm $ liftDyn2 (&&) (eval' env x) (eval' env y)
      -- B Cat x y  -> traceShowICFP env curTerm $ liftDyn2 @String (++) (eval' env x) (eval' env y)
      -- B Take x y -> traceShowICFP env curTerm $ liftDyn2 @Integer @String take (eval' env x) (eval' env y)
      -- B Drop x y -> traceShowICFP env curTerm $ liftDyn2 @Integer @String drop (eval' env x) (eval' env y)
      -- If b t f   -> traceShowICFP env curTerm $ if fromDynUnsafe @Bool (eval' env b)
      --                           then eval' env t
      --                           else eval' env f
      -- B App x y  -> traceShowICFP env curTerm $ f y
      --   where
      --     f = fromDynUnsafe @(ICFP Unique -> Dynamic) (eval' env x)
      -- V v        -> traceShowICFP env curTerm $ eval' env (env M.! v)
      -- L v body   -> traceShowICFP env curTerm $ toDyn (\x -> eval' (M.insert v x env) body)
      T          -> pure $ toDyn True
      F          -> pure $ toDyn False
      I z        -> pure $ toDyn z
      S s        -> pure $ toDyn s
      U Neg z    -> liftDyn @Integer negate (eval' env z)
      U Not b    -> liftDyn @Bool not (eval' env b)
      U S2I s    -> liftDyn string2int (eval' env s)
      U I2S z    -> liftDyn int2string (eval' env z)
      B Add x y  -> liftDyn2 @Integer (+) (eval' env x) (eval' env y)
      B Sub x y  -> liftDyn2 @Integer (-) (eval' env x) (eval' env y)
      B Mul x y  -> liftDyn2 @Integer (*) (eval' env x) (eval' env y)
      B Div x y  -> liftDyn2 @Integer quot (eval' env x) (eval' env y) -- required semantics are 'quot' instead of 'div'
      B Mod x y  -> liftDyn2 @Integer rem (eval' env x) (eval' env y) -- required semantics are 'rem' instead of 'mod'
      B LT x y   -> liftDyn2 @Integer (<) (eval' env x) (eval' env y) -- NOTE assuming strict
      B GT x y   -> liftDyn2 @Integer (>) (eval' env x) (eval' env y)
      B EQ x y   -> dynEq (eval' env x) (eval' env y)
      B Or x y   -> liftDyn2 (||) (eval' env x) (eval' env y)
      B And x y  -> liftDyn2 (&&) (eval' env x) (eval' env y)
      B Cat x y  -> liftDyn2 @String (++) (eval' env x) (eval' env y)
      B Take x y -> liftDyn2 @Integer @String take' (eval' env x) (eval' env y)
      B Drop x y -> liftDyn2 @Integer @String drop' (eval' env x) (eval' env y)
      If b t f   -> do
                      b' <- eval' env b
                      if fromDynUnsafe @Bool b'
                        then eval' env t
                        else eval' env f
      B App f y  -> do
                      f' <- eval' env f
                      fromDynUnsafe @(ICFP Unique -> IO Dynamic) f' y
      V v        -> eval' env (env M.! v)
      L v body   -> do
        v' <- newUnique
        let body' = alphaRename v v' body
        pure $ toDyn (\x -> eval' (M.insert v' x env) body')

alphaRename :: Unique -> Unique -> ICFP Unique -> ICFP Unique
alphaRename v v' = mapVar (\var -> if var == v then v' else var)

string2int :: String -> Integer
string2int = toIcfpString .> fromBase94

int2string :: Integer -> String
int2string = toBase94 .> fromIcfpString

take' :: Integer -> [a] -> [a]
take' n = take (fromInteger n)

drop' :: Integer -> [a] -> [a]
drop' n = drop (fromInteger n)

traceShowICFP :: Map Unique (ICFP Unique) -> ICFP Unique -> b -> b
traceShowICFP env term = trace debugStr
  where
    debugStr = env' ++ " |- " ++ term'
    env' = M.mapKeys hashUnique env
           |> fmap (mapVar (hashUnique .> fromIntegral) .> prettyAST)
           |> show
    term' = prettyAST $ mapVar (hashUnique .> fromIntegral) term

dynEq :: IO Dynamic -> IO Dynamic -> IO Dynamic
dynEq mx my = do
  x <- mx
  y <- my
  pure $ fromMaybe
      (error "Types of B Eq don't match or aren't Integer, Bool, or String")
      (   typedEq @Integer Proxy x y
      <|> typedEq @Bool Proxy x y
      <|> typedEq @String Proxy x y
      )
  where
    typedEq :: forall a. (Eq a, Typeable a) => Proxy a -> Dynamic -> Dynamic -> Maybe Dynamic
    typedEq _ x y = do
      x' <- fromDynamic @a x
      y' <- fromDynamic @a y
      Just (toDyn $ x' == y')



foldICFP :: ICFPFuncs var b -> ICFP var -> b
foldICFP (ICFPFuncs{..}) = go
  where
    go = \case
      T -> tF
      F -> fF
      I z -> iF z
      S s -> sF s
      U uop x -> uF uop (go x)
      B bop x y -> bF bop (go x) (go y)
      If b t f -> ifF (go b) (go t) (go f)
      V v -> vF v
      L v body -> lF v (go body)

data ICFPFuncs var b = ICFPFuncs
  { tF :: b
  , fF :: b
  , iF :: Integer -> b
  , sF :: String -> b
  , uF :: UOp -> b -> b
  , bF :: BOp -> b -> b -> b
  , ifF :: b -> b -> b -> b
  , lF :: var -> b -> b
  , vF :: var -> b
  }

mapVar :: (var -> var') -> ICFP var -> ICFP var'
mapVar f = foldICFP $ ICFPFuncs
  { tF = T
  , fF = F
  , iF = I
  , sF = S
  , uF = U
  , bF = B
  , ifF = If
  , lF = \v b -> L (f v) b
  , vF = \v -> V (f v)
  }

egICFP :: ICFP Var
egICFP = L 1 (L 4 $ B App (V 4) (V 1))

egICFP' :: ICFP Var
egICFP' = L 1 $ L 2 $ L 3 $ If (V 2) (V 1) (V 3)

egICFP'' :: ICFP Var
egICFP'' = B App (B App (L 1 (L 2 (V 1))) T) F

egICFP''' :: ICFP Var
egICFP''' = B App (L 1 (V 1)) T


-- test with `evalTo @Bool lazynessTest`
-- it works!
lazynessTest :: ICFP Var
lazynessTest = B App (B App (L 1 (L 2 (V 1))) T) loop

lazynessTest' :: ICFP Var
lazynessTest' = B App (B App (L 1 (L 2 (V 2))) loop) T

loop :: ICFP Var
loop = B App yComb (L 1 $ V 1)

yComb :: ICFP Var
yComb = L 1 $ B App (L 2 $ B App (V 1) (B App (V 2) (V 2)))
                    (L 2 $ B App (V 1) (B App (V 2) (V 2)))


alphaConvert :: ICFP Var -> IO (ICFP Unique)
alphaConvert term = do
  term' <- go M.empty (initEithers term)
  pure (extractAllUnique term')
  where
    initEithers :: ICFP Var -> ICFP (Either Var a)
    initEithers = mapVar Left
    
    go :: Map Var Unique -> ICFP (Either Var Unique) -> IO (ICFP (Either Var Unique))
    go alphaR = \case
      V (Right v) -> pure $ V (Right v)
      V (Left v) -> case alphaR M.!? v of
          Just uv -> pure $ V (Right uv)
          Nothing -> pure $ V (Left v)

      L v body -> case v of
        Right uv -> do
          body' <- go alphaR body
          pure $ L (Right uv) body'
        Left v' -> do
          -- first, recursively alpha convert body *before* adding this lambda
          -- this is so any shadowed variables are renamed first
          body' <- go alphaR body
          

          -- then, generate unique id and add to Map
          uv <- liftIO newUnique
          let alphaR' = M.insert v' uv alphaR

          -- finally, alpha rename with the new variable from this lambda
          body'' <- go alphaR' body'
          pure $ L (Right uv) body''

      U uop x -> do x' <- go alphaR x; pure $ U uop x'
      B bop x y -> do
        x' <- go alphaR x
        y' <- go alphaR y
        pure $ B bop x' y'
      If b t f -> do
        b' <- go alphaR b
        t' <- go alphaR t
        f' <- go alphaR f
        pure $ If b' t' f'
      T -> pure T
      F -> pure F
      I x -> pure $ I x
      S s -> pure $ S s

    extractAllUnique :: ICFP (Either Var Unique) -> ICFP Unique
    extractAllUnique = foldICFP $ ICFPFuncs
      { tF = T
      , fF = F
      , iF = I
      , sF = S
      , uF = U
      , bF = B
      , ifF = If
      , lF = \v b -> case v of
                Left _ -> error "Free variables left in term!"
                Right uv -> L uv b
                -- L (Left v) b
      , vF = \case
                Left _ -> error "Free variables left in term!"
                Right uv -> V uv
      }

fromDynUnsafe :: Typeable a => Dynamic -> a
fromDynUnsafe x = fromDyn x (error $ "Dynamic typing failed! Actual type:" ++ show (dynTypeRep x))

liftDyn :: forall a b. (Typeable a, Typeable b) => (a -> b) -> IO Dynamic -> IO Dynamic
liftDyn f mx = do
  x <- mx
  pure $ toDyn f `dynApp` x

liftDyn2 :: forall a b c. (Typeable a, Typeable b, Typeable c)
         => (a -> b -> c) -> IO Dynamic -> IO Dynamic -> IO Dynamic
liftDyn2 f mx my = do
  x <- mx
  y <- my
  pure $ (toDyn f `dynApp` x) `dynApp` y
