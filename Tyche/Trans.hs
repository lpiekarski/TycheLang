module Tyche.Trans where

import           Tyche.Abs
import           Tyche.Numerical
import           Tyche.Types

transIdent :: Ident -> ()
transIdent x = case x of
  Ident string -> ()
transArg :: Arg LineInfo -> ()
transArg x = case x of
  Arg _ argmod ident fulltype -> ()
transType :: Type LineInfo -> ()
transType x = case x of
  Int _                   -> ()
  Str _                   -> ()
  Bool _                  -> ()
  Void _                  -> ()
  Float _                 -> ()
  List _ fulltype         -> ()
  Array _ fulltype        -> ()
  Fun _ argtypes fulltype -> ()
transArgType :: ArgType LineInfo -> ()
transArgType x = case x of
  ArgType _ argmod fulltype -> ()
transFullType :: FullType LineInfo -> ()
transFullType x = case x of
  FullType _ typemods type_ -> ()
transArgMod :: ArgMod LineInfo -> ()
transArgMod x = case x of
  AModVar _   -> ()
  AModVal _   -> ()
  AModInOut _ -> ()
transTypeMod :: TypeMod LineInfo -> ()
transTypeMod x = case x of
  TModReadonly _ -> ()
transAddOp :: AddOp LineInfo -> Val -> Val -> (Val, ErrorType)
transAddOp x v1 v2 = case x of
  Plus _  -> (addNumericals v1 v2, NoErr)
  Minus _ -> (substractNumericals v1 v2, NoErr)
transMulOp :: MulOp LineInfo -> Val -> Val -> (Val, ErrorType)
transMulOp x v1 v2 = case x of
  Times _ -> (multiplyNumericals v1 v2, NoErr)
  Div _   -> case v2 of
    FloatVal 0 -> (NoVal, ErrMsg "Encountered division by 0\n")
    IntVal 0   -> (NoVal, ErrMsg "Encountered division by 0\n")
    otherwise  -> (divideNumericals v1 v2, NoErr)
  Mod _   -> case v2 of
    FloatVal 0 -> (NoVal, ErrMsg "Encountered division by 0\n")
    IntVal 0   -> (NoVal, ErrMsg "Encountered division by 0\n")
    otherwise  -> (modNumericals v1 v2, NoErr)
transRelOp :: RelOp LineInfo -> Val -> Val -> (Val, ErrorType)
transRelOp x v1 v2 = case x of
  LTH _ -> (lthNumericals v1 v2, NoErr)
  LE _  -> (leNumericals v1 v2, NoErr)
  GTH _ -> (gthNumericals v1 v2, NoErr)
  GE _  -> (geNumericals v1 v2, NoErr)
  EQU _ -> (equNumericals v1 v2, NoErr)
  NE _  -> (neNumericals v1 v2, NoErr)
transOrOp :: OrOp LineInfo -> Val -> Val -> (Val, ErrorType)
transOrOp x v1 v2 = case x of
  Or _ -> case (v1, v2) of
    (BoolVal x1, BoolVal x2) -> (BoolVal (x1 || x2), NoErr)
transAndOp :: AndOp LineInfo -> Val -> Val -> (Val, ErrorType)
transAndOp x v1 v2 = case x of
  And _ -> case (v1, v2) of
    (BoolVal x1, BoolVal x2) -> (BoolVal (x1 && x2), NoErr)
