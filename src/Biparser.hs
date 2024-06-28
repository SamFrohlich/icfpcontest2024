{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Biparser where

import Data.Profunctor

import Control.Monad
import Control.Applicative (Alternative(..))
import Flow

data Parser a = Parser { runParser :: String -> [(a, String)] }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ \s -> do
    (a, s') <- p s
    pure (f a, s')

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ \s -> [(a, s)]
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  Parser pf <*> Parser p = Parser $ \s -> do
    (f, s') <- pf s
    (a, s'') <- p s'
    pure (f a, s'')

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser p >>= f = Parser $ \s -> do
    (a, s') <- p s
    let Parser p' = f a
    p' s'

instance MonadFail Parser where
  fail :: String -> Parser a
  fail _ = Parser $ \_ -> []

instance Alternative Parser where
  empty :: Parser a
  empty = fail ""

  (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = Parser $ \s ->
    let res1 = runParser p1 s
    in if null res1
         then runParser p2 s
         else res1

parse :: Parser a -> String -> a
parse p
  = runParser p
 .> head
 .> fst

-- Printer

data Printer b a = Printer { runPrinter :: b -> Maybe (a, String) }

instance Functor (Printer b) where
  fmap :: (a -> c) -> Printer b a -> Printer b c
  fmap f (Printer p) = Printer $ \b -> do
    (a, s) <- p b
    return (f a, s)

instance Applicative (Printer b) where
  pure :: a -> Printer b a
  pure x = Printer  $ const (pure (x, ""))
  (<*>) :: Printer b (a -> c) -> Printer b a -> Printer b c
  Printer pf <*> Printer pa = Printer $ \b -> do
    (f, s1) <- pf b
    (a, s2) <- pa b
    return (f a, s1 <> s2)

instance Monad (Printer b) where
  (>>=) :: Printer b a -> (a -> Printer b c) -> Printer b c
  Printer pa >>= f = Printer $ \b -> do
    (a, s) <- pa b
    let Printer pc = f a
    (c, s') <- pc b
    return (c, s <> s')

data Biparser b a = BP
  { asParser :: !(Parser a)
  , asPrinter :: !(Printer b a)
  }

instance Profunctor Biparser where
  lmap :: (a -> b) -> Biparser b c -> Biparser a c
  lmap f (BP parse (Printer p)) = BP parse (Printer (\a -> p (f a)))
  rmap :: (b -> c) -> Biparser a b -> Biparser a c
  rmap f (BP parse print) = BP (fmap f parse) (fmap f print)

instance Functor (Biparser b) where
  fmap :: (a -> c) -> Biparser b a -> Biparser b c
  fmap = rmap

instance Applicative (Biparser b) where
  pure :: a -> Biparser b a
  pure x = BP (pure x) (pure x)
  (<*>) :: Biparser b (a -> c) -> Biparser b a -> Biparser b c
  BP p q <*> BP p' q' = BP (p <*> p') (q <*> q')

instance Monad (Biparser b) where
  (>>=) :: Biparser b a -> (a -> Biparser b c) -> Biparser b c
  BP p q >>= f = BP (p >>= asParser . f) (q >>= asPrinter . f)

class Profunctor p => PartialProfunctor p where
  prune :: p b a -> p (Maybe b) a
  comap :: (a -> Maybe b) -> p b v -> p a v
  comap f = lmap f . prune

instance PartialProfunctor Biparser where
  prune :: Biparser b a -> Biparser (Maybe b) a
  prune (BP parser (Printer print)) =
    BP parser
    (Printer $ \case { Nothing -> Nothing
                     ; Just b -> print b
                     })

class (PartialProfunctor p, forall b . Monad (p b)) => PMP p
instance (PartialProfunctor p, forall b . Monad (p b)) => PMP p
