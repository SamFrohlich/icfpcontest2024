{-# LANGUAGE GADTs, TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Eval where

import AST
import NumberMap

import Prelude hiding (Ordering(..))

import Control.Monad.Trans.State.Strict (StateT, get, evalStateT, modify)

import Data.Map (Map)
import Data.Map qualified as M

import Data.Dynamic (Dynamic (Dynamic), toDyn, fromDyn, Typeable, dynTypeRep, dynApp)
import Data.Unique
import Control.Monad.IO.Class (liftIO, MonadIO)

-- You pretty much always want to use this function,
-- using a type annotation to specify what type you expect the term to evaluate to,
-- e.g.`evalTo @Bool (B App (L 1 (V 1)) T)`
-- which will evaluate to `True`
-- NOTE: eval and evalTo only evaluates closed terms
evalTo :: Typeable a => ICFP Var -> IO a
evalTo term = fromDynUnsafe <$> eval term

-- The IO here is for alpha conversion, using GHC's built in
-- `newUnique :: IO Unique` to guarantee unique symbols
-- NOTE: there's no handling of errors atm because I couldn't be bothered
--       and there shouldn't be any unless there's a bug in my implementation or
--       they give us ill-formed or open terms 
eval :: ICFP Var -> IO Dynamic
eval term = do
  term' <- alphaConvert term
  pure $ eval' M.empty term'
  where
    eval' :: Map Unique (ICFP Unique) -> ICFP Unique -> Dynamic
    eval' _ T = toDyn True
    eval' _ F = toDyn False
    eval' _ (I z) = toDyn z
    eval' _ (S s) = toDyn s
    eval' mem (U Neg z) = liftDyn @Int negate (eval' mem z)
    eval' mem (U Not b) = liftDyn @Bool not (eval' mem b)
    eval' mem (U S2I s) = liftDyn fromBase94 (eval' mem s)
    eval' mem (U I2S z) = liftDyn toBase94 (eval' mem z)
    eval' mem (B Add x y) = liftDyn2 @Int (+) (eval' mem x) (eval' mem y)
    eval' mem (B Sub x y) = liftDyn2 @Int (-) (eval' mem x) (eval' mem y)
    eval' mem (B Mul x y) = liftDyn2 @Int (*) (eval' mem x) (eval' mem y)
    eval' mem (B Div x y) = liftDyn2 @Int div (eval' mem x) (eval' mem y)
    eval' mem (B Mod x y) = liftDyn2 @Int mod (eval' mem x) (eval' mem y)
    eval' mem (B LT x y) = liftDyn2 @Int (<) (eval' mem x) (eval' mem y) -- NOTE assuming strict
    eval' mem (B GT x y) = liftDyn2 @Int (>) (eval' mem x) (eval' mem y)
    eval' mem (B EQ x y) = liftDyn2 @Int (==) (eval' mem x) (eval' mem y)
    eval' mem (B Or x y) = liftDyn2 (||) (eval' mem x) (eval' mem y)
    eval' mem (B And x y) = liftDyn2 (&&) (eval' mem x) (eval' mem y)
    eval' mem (B Cat x y) = liftDyn2 @String (++) (eval' mem x) (eval' mem y)
    eval' mem (B Take x y) = liftDyn2 @Int @String take (eval' mem x) (eval' mem y)
    eval' mem (B Drop x y) = liftDyn2 @Int @String drop (eval' mem x) (eval' mem y)
    eval' mem (If b t f) = if fromDynUnsafe @Bool (eval' mem b)
                              then eval' mem t
                              else eval' mem f
    eval' mem (B App x y) = f y
      where
        f = fromDynUnsafe @(ICFP Unique -> Dynamic) (eval' mem x)
    eval' mem (V v) = eval' mem (mem M.! v)
    eval' mem (L v body) = toDyn (\x -> eval' (M.insert v x mem) body)

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
  , iF :: Int -> b
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

liftDyn :: forall a b. (Typeable a, Typeable b) => (a -> b) -> Dynamic -> Dynamic
liftDyn f x = toDyn f `dynApp` x

liftDyn2 :: forall a b c. (Typeable a, Typeable b, Typeable c) => (a -> b -> c) -> Dynamic -> Dynamic -> Dynamic
liftDyn2 f x y = (toDyn f `dynApp` x) `dynApp` y
