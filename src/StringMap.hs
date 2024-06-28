module StringMap where

import Data.Map (Map)
import Data.Map qualified as M

convertString :: String -> String
convertString = map toICFPChar

toICFPChar :: Char -> Char
toICFPChar = (charMap M.!)

charMap :: Map Char Char
charMap = M.fromList
  (zip ['!'.. '~']
       "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n")
