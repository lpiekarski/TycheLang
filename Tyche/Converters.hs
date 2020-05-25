module Tyche.Converters where

import           Tyche.Abs
import           Tyche.Types

argsToArgTypes :: [Arg a] -> [ArgType a]
argsToArgTypes [] = []
argsToArgTypes ((Arg li am fi ft):args) =
  (ArgType li am ft):(argsToArgTypes args)

argsToFullTypes :: [Arg (Maybe (Int, Int))] -> [FullType (Maybe (Int, Int))]
argsToFullTypes args =
  let
    go [] r =
      reverse r
    go (a:as) r =
      case a of
        Arg _ _ _ fulltype ->
          go as (fulltype:r)
  in
    go args []

argToArgVal :: Arg (Maybe (Int, Int)) -> VEnv -> State -> Maybe ArgVal
argToArgVal arg venv ((_, storef), _, _) =
  case arg of
    Arg lineInfo argmod ident fulltype ->
      case argmod of
        AModVar _ ->
          case venv ident of
            Nothing ->
              Nothing
            Just loc ->
              Just (Variable loc ident)
        AModVal _ ->
          case venv ident of
            Nothing ->
              Nothing
            Just loc ->
              Just (Value (storef loc) ident)
        AModInOut _ ->
          Nothing

argTypeToFullType :: ArgType LineInfo -> FullType LineInfo
argTypeToFullType (ArgType _ _ res) = res

argsToArgVals :: [Arg (Maybe (Int, Int))] -> VEnv -> State -> [ArgVal]
argsToArgVals args venv (state@((_, storef), _, _)) =
  let
    go [] r =
      reverse r
    go (a:as) r = case argToArgVal a venv state of
        Nothing  -> go as r
        Just res -> go as (res:r)
  in
    go args []
