module Tyche.Types where

import Tyche.ErrM
import Tyche.Abs

import Data.Array
import System.IO ( stdin, hGetContents )

extendFunc :: (Eq a) => (a -> b) -> a -> b -> a -> b
extendFunc f nx ny x = if nx == x then ny else f x

extendFuncArgs :: TEnv -> [Arg (Maybe (Int, Int))] -> TEnv
extendFuncArgs te [] = te
extendFuncArgs te ((Arg _ _ ident ft):args) =
      extendFuncArgs (extendFunc te ident (Just ft)) args

lineInfoString :: Maybe (Int, Int) -> String
lineInfoString Nothing = ""
lineInfoString (Just (x, y)) = show x ++ ":" ++ show y

matchArgTypes :: [ArgType a] -> [ArgType a] -> Bool
matchArgTypes [] [] = True
matchArgTypes (at1:ats1) [] = False
matchArgTypes [] (at2:ats2) = False
matchArgTypes (at1:ats1) (at2:ats2) = case (at1, at2) of
  (ArgType _ _ ft1, ArgType _ _ ft2) -> (matchFullType ft1 ft2) && (matchArgTypes ats1 ats2)


matchType :: Type a -> Type a -> Bool
matchType t1 t2 = case (t1, t2) of
  (Int _, Int _) -> True
  (Float _, Float _) -> True
  (Str _, Str _) -> True
  (Bool _, Bool _) -> True
  (Void _, Void _) -> True
  (List _ ft1, List _ ft2) -> matchFullType ft1 ft2
  (Array _ ft1, Array _ ft2) -> matchFullType ft1 ft2
  (Fun _ ats1 ft1, Fun _ ats2 ft2) -> (matchArgTypes ats1 ats2) && (matchFullType ft1 ft2)
  otherwise -> False

matchFullType :: FullType a -> FullType a -> Bool
matchFullType ft1 ft2 = case (ft1, ft2) of
  (FullType _ _ t1, FullType _ _ t2) -> matchType t1 t2
  otherwise -> False

argsToArgTypes :: [Arg a] -> [ArgType a]
argsToArgTypes [] = []
argsToArgTypes ((Arg li am fi ft):args) =
  (ArgType li am ft):(argsToArgTypes args)

isReadonly :: FullType a -> Bool
isReadonly ft = case ft of
  FullType _ tms _ ->
    let
      isRo x = case x of
        TModReadonly _ -> True
        otherwise -> False
    in
      any isRo tms
  otherwise -> False

isArray :: FullType a -> Bool
isArray ft = case ft of
  FullType _ _ (Array _ _) -> True
  otherwise -> False

isList :: FullType a -> Bool
isList ft = case ft of
  FullType _ _ (List _ _) -> True
  otherwise -> False

isBool :: FullType a -> Bool
isBool ft = case ft of
  FullType _ _ (Bool _) -> True
  otherwise -> False

isInt :: FullType a -> Bool
isInt ft = case ft of
  FullType _ _ (Int _) -> True
  otherwise -> False

isFloat :: FullType a -> Bool
isFloat ft = case ft of
  FullType _ _ (Float _) -> True
  otherwise -> False

isVoid :: FullType a -> Bool
isVoid ft = case ft of
  FullType _ _ (Void _) -> True
  otherwise -> False

isFunction :: FullType a -> Bool
isFunction ft = case ft of
  FullType _ _ (Fun _ _ _) -> True
  otherwise -> False

arrayElementType :: FullType a -> Maybe (FullType a)
arrayElementType ft = case ft of
  FullType _ _ (Array _ res) -> Just res
  otherwise -> Nothing

listElementType :: FullType a -> Maybe (FullType a)
listElementType ft = case ft of
  FullType _ _ (List _ res) -> Just res
  otherwise -> Nothing

elementType :: FullType a -> Maybe (FullType a)
elementType ft = case ft of
  FullType _ _ (List _ res) -> Just res
  FullType _ _ (Array _ res) -> Just res
  otherwise -> Nothing

newLoc :: Store -> (Loc, Store)
newLoc (next, s) = (next, (next + 1, s))

saveInStore :: Store -> Loc -> Val -> Store
saveInStore (l, s) loc val = (l, \loc' -> if loc' == loc then val else s loc')

addBreakLabel :: LEnv -> IO ECont -> LEnv
addBreakLabel (LEnv lenv) econt =
  LEnv (\label -> case label of
    LBreak -> Just econt
    otherwise -> lenv label)

addContinueLabel :: LEnv -> IO ECont -> LEnv
addContinueLabel (LEnv lenv) econt =
  LEnv (\label -> case label of
    LContinue -> Just econt
    otherwise -> lenv label)

addReadonly :: FullType (Maybe (Int, Int)) -> FullType (Maybe (Int, Int))
addReadonly ft@(FullType li tms t) =
  if isReadonly ft then
    ft
  else
    FullType li ((TModReadonly Nothing):tms) t

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


printState :: State -> IO ()
printState (store, err) = do
  putStrLn "store:"
  printStore store
  putStrLn "error:"
  putStrLn (show err)
  return ()

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
argToArgVal arg venv ((_, storef), _) =
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

argsToArgVals :: [Arg (Maybe (Int, Int))] -> VEnv -> State -> [ArgVal]
argsToArgVals args venv (state@((_, storef), _)) =
  let
    go [] r =
      reverse r
    go (a:as) r = case argToArgVal a venv state of
        Nothing -> go as r
        Just res -> go as (res:r)
  in
    go args []

type Loc = Int
data ArgVal = Variable Loc Ident | Value Val Ident | Inout Ident Ident
data Val = IntVal Integer | FloatVal Double | BoolVal Bool | StringVal String | ListVal [Val] | ArrayVal (Array Int Val) | FuncVal ([ArgType (Maybe (Int, Int))] -> [ArgVal] -> IO ICont -> IO Cont) | NoVal
instance Show Val where
  show val = case val of
    IntVal integer -> "int " ++ (show integer)
    FloatVal double -> "float " ++ (show double)
    BoolVal bool -> "bool " ++ (show bool)
    StringVal str -> "string " ++ str
    ListVal l -> "list " ++ (show l)
    ArrayVal (_) -> "array"
    FuncVal (_) -> "function"
    NoVal -> "empty"
type Var = Ident
type Store = (Loc, Loc -> Val)
data Error =
  NoError (Maybe (Int, Int)) |
  DivisionBy0 (Maybe (Int, Int)) |
  TypeError (Maybe (Int, Int)) |
  BreakError (Maybe (Int, Int)) |
  ContinueError (Maybe (Int, Int)) |
  ReturnError (Maybe (Int, Int)) |
  LoopError (Maybe (Int, Int))
  deriving (Show)
type State = (Store, Error)
type Cont = State -> IO State
type ICont = TEnv -> VEnv -> IO Cont
type ECont = FullType (Maybe (Int, Int)) -> Val -> IO Cont
type VEnv = Var -> Maybe Loc
data Label = LBreak | LContinue | LReturn | LProb Int Int Int
data LEnv = LEnv (Label -> Maybe (IO ECont))
type Scope = (Maybe (FullType (Maybe (Int, Int))), Bool, Int)
type TypeCheckResult = Err (TEnv, Maybe (FullType (Maybe (Int, Int))), Scope)
type TEnv = Var -> Maybe (FullType (Maybe (Int, Int)))

readonlyVoidT = FullType Nothing [TModReadonly Nothing] (Void Nothing)
voidT = FullType Nothing [] (Void Nothing)
readonlyBoolT = FullType Nothing [TModReadonly Nothing] (Bool Nothing)
boolT = FullType Nothing [] (Bool Nothing)
readonlyIntT = FullType Nothing [TModReadonly Nothing] (Int Nothing)
intT = FullType Nothing [] (Int Nothing)
readonlyStringT = FullType Nothing [TModReadonly Nothing] (Str Nothing)
stringT = FullType Nothing [] (Str Nothing)
readonlyFloatT = FullType Nothing [TModReadonly Nothing] (Float Nothing)
floatT = FullType Nothing [] (Float Nothing)
readonlyListT fulltype = FullType Nothing [TModReadonly Nothing] (List Nothing fulltype)
listT fulltype = FullType Nothing [] (List Nothing fulltype)

{-typeOf :: Val -> FullType (Maybe (Int, Int))
typeOf IntVal _ = readonlyIntT
typeOf FloatVal _
BoolVal _
StringVal _
ListVal (_, Val, _)
ArrayVal (Array Int Val)
FuncVal (Cont -> Cont)
NoVal-}

negateNum :: Val -> Val
negateNum (IntVal v) = IntVal (-v)
negateNum (FloatVal v) = FloatVal (-v)
negateNum v = v

negateBool :: Val -> Val
negateBool (BoolVal v) = BoolVal (not v)
negateBool v = v
