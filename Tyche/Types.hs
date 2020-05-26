module Tyche.Types where

import           Tyche.Abs
import           Tyche.ErrM

import           Data.Array
import           System.IO  (hGetContents, stdin)

stringToInput :: String -> Input
stringToInput str =
  let
    go [] acc     = acc
    go (c:cs) acc = go cs (Input c acc)
  in
    go (reverse str) EOI

stringToOutput :: String -> Output -> Output
stringToOutput str acc =
  let
    go [] acc     = acc
    go (c:cs) acc = go cs (Output c acc)
  in
    go (reverse str) acc

outputToString :: Output -> String
outputToString output =
  let
    go EOO acc          = acc
    go (Output c o) acc = go o (c:acc)
  in
    go output ""

typeOf :: Val -> FullType LineInfo
typeOf val = case val of
  IntVal _     -> intT
  FloatVal _   -> floatT
  BoolVal _    -> boolT
  StringVal _  -> stringT
  ListVal list -> voidT
  ArrayVal arr -> voidT
  FuncVal func -> voidT
  NoVal        -> voidT

type Loc = Int
data Input = Input Char Input
    | EOI
data Output = Output Char Output
    | EOO
data ArgVal = Variable Loc Ident
    | Value Val Ident
    | Inout Ident Ident
data Val = IntVal Integer
    | FloatVal Double
    | BoolVal Bool
    | StringVal String
    | ListVal [Val]
    | ArrayVal (Array Int Val)
    | FuncVal (LEnv -> ICont -> Cont)
    | NoVal
instance Show Val where
  show val = case val of
    IntVal integer  -> "int " ++ (show integer)
    FloatVal double -> "float " ++ (show double)
    BoolVal bool    -> "bool " ++ (show bool)
    StringVal str   -> "string " ++ str
    ListVal l       -> "list " ++ (show l)
    ArrayVal a      -> "array " ++ (show a)
    FuncVal (_)     -> "function"
    NoVal           -> "void"
type Store = (Loc, Loc -> Val)
type StackTrace = [Expr LineInfo]
data ErrorType = NoErr
    | ErrMsg String
type State = (Store, StackTrace, Input)
type Ans = (ErrorType, StackTrace, Output)
type Cont = State -> Ans
type ICont = VEnv -> Cont
type ECont = Val -> Cont
type VEnv = Ident -> Maybe Loc
data Label = LBreak
    | LContinue
    | LReturn
    | LProb Int Int Int
type LEnv = (Label -> Maybe (ECont))
type TypeCheckResult = Err (FullType LineInfo, TEnv, FullType LineInfo, Bool, Bool)
type TEnv = Ident -> Maybe (FullType LineInfo)
type LineInfo = Maybe (Int, Int)

errMsg :: String -> State -> Ans
errMsg str (store, stacktrace, input) = (ErrMsg str, stacktrace, EOO)

readonlyVoidT = FullType Nothing [TModReadonly Nothing] (Void Nothing)
voidT = FullType Nothing [] (Void Nothing)
readonlyBoolT = FullType Nothing [TModReadonly Nothing] (Bool Nothing)
boolT :: FullType LineInfo
boolT = FullType Nothing [] (Bool Nothing)
readonlyIntT = FullType Nothing [TModReadonly Nothing] (Int Nothing)
intT = FullType Nothing [] (Int Nothing)
readonlyStringT = FullType Nothing [TModReadonly Nothing] (Str Nothing)
stringT = FullType Nothing [] (Str Nothing)
readonlyFloatT = FullType Nothing [TModReadonly Nothing] (Float Nothing)
floatT = FullType Nothing [] (Float Nothing)
readonlyListT fulltype = FullType Nothing [TModReadonly Nothing] (List Nothing fulltype)
listT fulltype = FullType Nothing [] (List Nothing fulltype)
arrayT fulltype = FullType Nothing [] (Array Nothing fulltype)
valArgT = ArgType Nothing (AModVal Nothing)
varArgT = ArgType Nothing (AModVar Nothing)
inoutArgT = ArgType Nothing (AModVar Nothing)
readonlyFunctionT args fulltype = FullType Nothing [TModReadonly Nothing] (Fun Nothing args fulltype)
functionT args fulltype = FullType Nothing [] (Fun Nothing args fulltype)

isReadonly :: FullType a -> Bool
isReadonly ft = case ft of
  FullType _ tms _ ->
    let
      isRo x = case x of
        TModReadonly _ -> True
        otherwise      -> False
    in
      any isRo tms
  otherwise -> False

isArray :: FullType a -> Bool
isArray ft = case ft of
  FullType _ _ (Array _ _) -> True
  otherwise                -> False

isList :: FullType a -> Bool
isList ft = case ft of
  FullType _ _ (List _ _) -> True
  otherwise               -> False

isBool :: FullType a -> Bool
isBool ft = case ft of
  FullType _ _ (Bool _) -> True
  otherwise             -> False

isInt :: FullType a -> Bool
isInt ft = case ft of
  FullType _ _ (Int _) -> True
  otherwise            -> False

isFloat :: FullType a -> Bool
isFloat ft = case ft of
  FullType _ _ (Float _) -> True
  otherwise              -> False

isVoid :: FullType a -> Bool
isVoid ft = case ft of
  FullType _ _ (Void _) -> True
  otherwise             -> False

isNumeric :: FullType a -> Bool
isNumeric ft = case ft of
  FullType _ _ (Float _) -> True
  FullType _ _ (Int _)   -> True
  otherwise              -> False

isFunction :: FullType a -> Bool
isFunction ft = case ft of
  FullType _ _ (Fun _ _ _) -> True
  otherwise                -> False

arrayElementType :: FullType a -> FullType a
arrayElementType (FullType _ _ (Array _ res)) = res

listElementType :: FullType a -> FullType a
listElementType (FullType _ _ (List _ res)) = res

elementType :: FullType a -> FullType a
elementType (FullType _ _ (List _ res))  = res
elementType (FullType _ _ (Array _ res)) = res

addReadonly :: FullType (Maybe (Int, Int)) -> FullType (Maybe (Int, Int))
addReadonly ft@(FullType li tms t) =
  if isReadonly ft then
    ft
  else
    FullType li ((TModReadonly Nothing):tms) t
