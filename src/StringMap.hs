module StringMap where

import Data.Bimap (Bimap)
import Data.Bimap qualified as M

fromIcfpString :: String -> String
fromIcfpString = map fromIcfpChar

toIcfpString :: String -> String
toIcfpString = map toIcfpChar

fromIcfpChar :: Char -> Char
fromIcfpChar = (charMap M.!)

toIcfpChar :: Char -> Char
toIcfpChar = (charMap M.!>)

charMap :: Bimap Char Char
charMap = M.fromList
  (zip ['!'.. '~']
       "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n")
