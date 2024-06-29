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

evalTo :: Typeable a => ICFP Var -> IO a
evalTo term = fromDynUnsafe <$> eval term

eval :: ICFP Var -> IO Dynamic
eval term = do
  term' <- alphaConvert term
  pure $ eval' M.empty term'
  where
    eval' :: Map Unique (ICFP Unique) -> (ICFP Unique) -> Dynamic
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
        f = fromDynUnsafe @(ICFP Unique -> Dynamic) (eval' mem x) -- Might need to do more here to ensure correct semantics
    eval' mem (V v) = eval' mem (mem M.! v)
    eval' mem (L v body) = toDyn (\x -> eval' (M.insert v x mem) body) -- eval' (M.insert )

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

yComb :: ICFP Var
yComb = L 1 $ B App (L 2 $ B App (V 1) (B App (V 2) (V 2)))
                    (L 2 $ B App (V 1) (B App (V 2) (V 2)))

fac :: ICFP Var
fac = B App yComb (L 1 $ L 2 $ If (lazyB EQ (V 2) (I 0))
                                  (I 1)
                                  (lazyB Mul (V 2) (B App (V 1) (lazyB Sub (V 2) (I 1)))))

-- Not lazy enough to actually do this, but I don't think we actually need this
lazyB :: BOp -> ICFP Var -> ICFP Var -> ICFP Var
lazyB op x y = B App (B App (L 1 $ L 2 $ B op (V 1) (V 2)) x) y

facEg :: ICFP Var
facEg = B App fac (I 0) -- 3!

-- test with `evalTo @Bool lazynessTest`
-- it works!
lazynessTest :: ICFP Var
lazynessTest = B App (B App (L 1 (L 2 (V 1))) T) loop

lazynessTest' :: ICFP Var
lazynessTest' = B App (B App (L 1 (L 2 (V 2))) loop) T

loop :: ICFP Var
loop = B App yComb (L 1 $ V 1)

alphaConvert :: ICFP Var -> IO (ICFP Unique)
alphaConvert term = do
  term' <- go M.empty (initEithers term)
  pure (extractAllUnique term')
  -- term' <- evalStateT (go (initEithers term)) M.empty
  -- pure (extractAllUnique term')
  where
    initEithers :: ICFP Var -> ICFP (Either Var a)
    initEithers = mapVar Left
    
    debug :: ICFP (Either Var Unique) -> StateT (Map Var Unique) IO ()
    debug termy = do
      env <- get
      liftIO $ print (fmap hashUnique env)
      liftIO $ print (mapVar (fmap hashUnique) termy)

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
          -- debug body
          -- first, recursively alpha convert body *before* adding this lambda
          -- this is so any shadowed variables are renamed first
          body' <- go alphaR body
          

          -- then, generate unique id and add to Map
          uv <- liftIO newUnique
          let alphaR' = M.insert v' uv alphaR

          -- debug body'

          -- finally, alpha rename with the new variable from this lambda
          body'' <- go alphaR' body'
          -- debug body''
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

    -- go :: ICFP (Either Var Unique) -> StateT (Map Var Unique) IO (ICFP (Either Var Unique))
    -- go termy = do
    --   -- alphaR <- get
    --   -- liftIO $ print (hashUnique <$> alphaR)
    --   case termy of
    --     V (Right v) -> pure $ V (Right v)
    --     V (Left v) -> do
    --       alphaR <- get
    --       case alphaR M.!? v of
    --         Just uv -> pure $ V (Right uv)
    --         Nothing -> pure $ V (Left v)

    --     L v body -> case v of
    --       Right uv -> do
    --         body' <- go body
    --         pure $ L (Right uv) body'
    --       Left v' -> do
    --         -- debug body
    --         -- first, recursively alpha convert body *before* adding this lambda
    --         -- this is so any shadowed variables are renamed first
    --         body' <- go body
            

    --         -- then, generate unique id and add to Map
    --         uv <- liftIO newUnique
    --         modify (M.insert v' uv)

    --         -- debug body'

    --         -- finally, alpha rename with the new variable from this lambda
    --         body'' <- go body'
    --         -- debug body''
    --         pure $ L (Right uv) body''

    --     U uop x -> do x' <- go x; pure $ U uop x'
    --     B bop x y -> do
    --       x' <- go x
    --       y' <- go y
    --       pure $ B bop x' y'
    --     If b t f -> do
    --       b' <- go b
    --       t' <- go t
    --       f' <- go f
    --       pure $ If b' t' f'
    --     T -> pure T
    --     F -> pure F
    --     I x -> pure $ I x
    --     S s -> pure $ S s


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
