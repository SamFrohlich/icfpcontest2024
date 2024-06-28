{-# LANGUAGE LambdaCase #-}
module SpaceshipsAuto where

import WebRepl

import Biparser
import Control.Applicative (Alternative(..))
import Data.Char (isDigit, isSpace)
import Control.Monad (void)
import Spaceships
import Flow

solveSpaceship :: Int -> IO ()
solveSpaceship n = do
  let msg = "get spaceship" ++ show n
  res <- sendMessage msg
  print (processResponse res)

processResponse :: String -> [CMD]
processResponse
  = parse pointsP
 .> pointsToCMDs


-- Parser

pointsP :: Parser [(Int, Int)]
pointsP = many (coord <* whitespace)

coord :: Parser (Int, Int)
coord = (,) <$> num <* whitespace1 <*> num

whitespace :: Parser ()
whitespace = void $ many (satisfy isSpace)

whitespace1 :: Parser ()
whitespace1 = void $ some (satisfy isSpace)

num :: Parser Int
num
  = read <$> (   liftA2 (:) (char '-') digits
             <|> digits)

digits :: Parser String
digits = some digit

char :: Char -> Parser Char
char c = satisfy (c ==)

digit :: Parser Char
digit = satisfy isDigit

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \case
  [] -> empty
  (c:cs) -> if p c then pure (c, cs) else empty

-- skip :: Parser ()
-- skip = Parser $ \(c:cs) -> ((), cs)

-- lookahead :: Parser Char
-- lookahead = Parser $ \(c:cs) -> (c, c:cs)