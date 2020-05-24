module Tyche.State where

import           Tyche.Types

newLoc :: Store -> (Loc, Store)
newLoc (next, s) = (next, (next + 1, s))

saveInStore :: Store -> Loc -> Val -> Store
saveInStore (l, s) loc val = (l, \loc' -> if loc' == loc then val else s loc')

printStore :: Store -> IO ()
printStore store =
  let
    go (next, s) i =
      if i == next then
        return ()
      else do
        putStrLn ((show i) ++ ": " ++ (show (s i)))
        go (next, s) (i + 1)
  in
    go store 0


printError :: Error -> IO ()
printError err = putStrLn $ show err
