module Timeprogramming where

import StringMap
import WebRepl

import Data.List

factorial :: String
factorial =
  (unlines
  ["solve 3d1"
  ,". . . . -1"
  ,". A > . + . > . > . > ."
  ,"1 = . . . . . . . . 10 @ -1"
  ,". . . . v . . . . . .  5"
  ,"A * S . . . . . . . 0"
  ,". . . A * . . . . A ="
  ,". . . < . > . . . . ."
  ,". 2 @ -3 . 3 @ -2 1 + S"
  ,". . 5 . . .  5"
  ]
  )

absolutevalue :: String
absolutevalue =
  (intercalate "\n"
   ["solve 3d2"
   ,". . . . . . -1 . 0  . . .  ."
   ,". A > . > .  + . #  . > .  ."
   ,". * . . . .  . . 1  . 8 @ -1"
   ,". S . . . 0  = . +  . . 5  ."
   ,". . . . . .  . . .  . . .  ."
   ,". . . . . .  . 8 @ -3 . .  ."
   ,". . . . . .  . . 5  . . .  ."
   ,". . . . . .  1 . 0  . . .  ."
   ,". A > . > .  + . #  . > .  ."
   ,". * . . . .  . . -1 . 8 @ -1"
   ,". S . . . 0  = . +  . . 5  ."
   ,". . . . . .  . . .  . . .  ."
   ,". . . . . .  . 8 @ -3 . .  ."
   ,". . . . . .  . . 5  . . .  ."
   ,". A ."
   ,"0 = S"
   ]
  )
