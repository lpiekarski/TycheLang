module Tyche.Env where

import           Tyche.Abs
import           Tyche.Types

extendFunc :: (Eq a) => (a -> b) -> a -> b -> a -> b
extendFunc f nx ny x = if nx == x then ny else f x

mergeVEnv :: VEnv -> VEnv -> VEnv
mergeVEnv first second = \x ->
  case second x of
    Just val -> Just val
    Nothing  -> first x

extendVEnv :: VEnv -> Ident -> Loc -> VEnv
extendVEnv venv var loc = \x ->
  if x == var then Just loc
  else venv x

extendTEnv :: TEnv -> Ident -> FullType LineInfo -> TEnv
extendTEnv tenv var fulltype = \v ->
  if v == var then Just fulltype
  else tenv v

extendTEnvArgs :: TEnv -> [Arg (Maybe (Int, Int))] -> TEnv
extendTEnvArgs te [] = te
extendTEnvArgs te ((Arg _ _ ident ft):args) =
      extendTEnvArgs (extendTEnv te ident ft) args

addBreakLabel :: LEnv -> ECont -> LEnv
addBreakLabel lenv econt = \label ->
  case label of
    LBreak    -> Just econt
    otherwise -> lenv label

addContinueLabel :: LEnv -> ECont -> LEnv
addContinueLabel lenv econt = \label ->
  case label of
    LContinue -> Just econt
    otherwise -> lenv label

addReturnLabel :: LEnv -> ECont -> LEnv
addReturnLabel lenv econt = \label ->
  case label of
    LReturn   -> Just econt
    otherwise -> lenv label
