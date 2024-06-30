module Parser where

-- base
import Prelude hiding (Ordering(..))

-- parsec
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token

-- local
import AST
import NumberMap
import StringMap

-- >>> parse uop "" "U-"
-- Right Neg
uop :: Parser UOp
uop = char 'U' *>
  (   try (Neg <$ char '-')
  <|> try (Not <$ char '!')
  <|> try (S2I <$ char '#')
  <|> try (I2S <$ char '$')
  <?> "uop")

bop :: Parser BOp
bop = char 'B' *>
  (   try (Add <$ char '+')
  <|> try (Sub <$ char '-')
  <|> try (Mul <$ char '*')
  <|> try (Div <$ char '/')
  <|> try (Mod <$ char '%')
  <|> try (LT  <$ char '<')
  <|> try (GT  <$ char '>')
  <|> try (EQ  <$ char '=')
  <|> try (Or  <$ char '|')
  <|> try (And <$ char '&')
  <|> try (Cat <$ char '.')
  <|> try (Take <$ char 'T')
  <|> try (Drop <$ char 'D')
  <|> (App <$ char '$')
  <?> "bop")

icfp :: Parser (ICFP Integer)
icfp =  buildExpressionParser table term

-- leaves
term =  try (T <$ char 'T')
    <|> try (F <$ char 'F')
    <|> try (char 'I' *> ((I . fromBase94) <$> manyTill anyChar (char ' ')) )
    <|> try (char 'S' *> ((S . fromIcfpString) <$> manyTill anyChar (char ' ')) )
    <|> try (char 'v' *> ((V . fromBase94) <$> manyTill anyChar (char ' ')) )

-- ops
table = [[ unary "U-" (U Neg)
         , unary "U!" (U Not)
         , unary "U#" (U S2I)
         , unary "U$" (U I2S)
        --  , binary "B+" (B Add)
        -- TODO operations dont support binary prefix
          ]]


unary name fun = Prefix (do { try (string name); return fun })
binary name fun = Prefix (do { try (string name); return fun })

