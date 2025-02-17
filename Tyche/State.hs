module Tyche.State where

import           Tyche.Print
import           Tyche.Types

newLoc :: Store -> (Loc, Store)
newLoc (next, s) = (next, (next + 1, s))

saveInStore :: Store -> Loc -> Val -> Store
saveInStore (l, s) loc val = (l, \loc' -> if loc' == loc then val else s loc')

printStore :: Store -> String
printStore store =
  let
    go :: Store -> Int -> String -> String
    go (next, s) i acc =
      if i == next then acc
      else go (next, s) (i + 1) (acc ++ (show i) ++ ": " ++ (show (s i)) ++ "\n")
  in
    go store 0 ""
