module Tyche.Helpers where

import           Tyche.Abs
import           Tyche.ErrM
import           Tyche.Types

passWithErrorHandle :: TypeCheckResult -> (a -> TEnv -> FullType LineInfo -> Bool -> Bool -> TypeCheckResult) -> a -> LineInfo -> TypeCheckResult
passWithErrorHandle x func next lineInfo = case x of
  Ok (_, tenv, functype, returned, inloop) -> func next tenv functype returned inloop
  Bad str -> Bad str

lineInfoString :: Maybe (Int, Int) -> String
lineInfoString Nothing       = "unknown location"
lineInfoString (Just (x, y)) = "line: " ++ show x ++ ", column: " ++ show y
