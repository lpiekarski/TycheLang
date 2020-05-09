module Tyche.TypeCheck where

import Tyche.Abs
import Tyche.ErrM
import Tyche.Print
import Tyche.Types

typecheckIdent :: Ident -> TEnv -> TypeCheckResult
typecheckIdent x te = case x of
  Ident string -> Ok (te, Nothing)
typecheckProgram :: Program (Maybe (Int, Int)) -> TypeCheckResult
typecheckProgram x = case x of
  Program _ stmt -> typecheckStmt stmt (\v -> Nothing)
typecheckArg :: Arg (Maybe (Int, Int)) -> TEnv -> TypeCheckResult
typecheckArg x te = case x of
  Arg _ argmod fullident fulltype -> typecheckfailure x
typecheckFullIdent :: FullIdent (Maybe (Int, Int)) -> TEnv -> TypeCheckResult
typecheckFullIdent x te = case x of
  FullIdent _ ident -> typecheckfailure x
  AnonIdent _ -> typecheckfailure x
typecheckStmt :: Stmt (Maybe (Int, Int)) -> TEnv -> TypeCheckResult
typecheckStmt x te = case x of
  Skip _ -> typecheckfailure x
  Break _ -> typecheckfailure x
  Continue _ -> typecheckfailure x
  Ret _ expr -> typecheckfailure x
  VRet _ -> typecheckfailure x
  VarDef lineInfo fullident fulltype expr ->
    case typecheckExpr expr te of
      Bad str -> Bad str
      Ok (te2, res) ->
        case res of
          Nothing -> Bad ("at " ++ (lineInfoString lineInfo) ++ " Expression has no type")
          Just ft -> if (matchFullType fulltype ft) then
            Ok (extendFunc te2 fullident (Just fulltype), Nothing)
          else
            Bad ("at " ++ (lineInfoString lineInfo) ++ " Can't assign type " ++ (printTree ft) ++ " to type " ++ (printTree fulltype))
  Ass _ ident expr -> typecheckfailure x
  FnDef _ fullident fulltype args stmt -> typecheckfailure x
  Cond _ expr stmt -> typecheckfailure x
  CondElse _ expr stmt1 stmt2 -> typecheckfailure x
  While _ expr stmt -> typecheckfailure x
  ForList _ ident expr stmt -> typecheckfailure x
  ForRange _ ident expr1 expr2 stmt -> typecheckfailure x
  Composition _ stmt1 stmt2 ->
    case typecheckStmt stmt1 te of
      Bad str -> Bad str
      Ok (te2, _) ->
        case typecheckStmt stmt2 te2 of
          Bad str -> Bad str
          Ok (te3, res) -> Ok (te3, res)
typecheckType :: Type (Maybe (Int, Int)) -> TEnv -> TypeCheckResult
typecheckType x te = case x of
  Int _ -> typecheckfailure x
  Str _ -> typecheckfailure x
  Bool _ -> typecheckfailure x
  Void _ -> typecheckfailure x
  Float _ -> typecheckfailure x
  List _ fulltype -> typecheckfailure x
  Array _ fulltype -> typecheckfailure x
  Fun _ argtypes fulltype -> typecheckfailure x
typecheckArgType :: ArgType (Maybe (Int, Int)) -> TEnv -> TypeCheckResult
typecheckArgType x te = case x of
  ArgType _ argmod type_ -> typecheckfailure x
typecheckFullType :: FullType (Maybe (Int, Int)) -> TEnv -> TypeCheckResult
typecheckFullType x te = case x of
  FullType _ typemods type_ -> typecheckfailure x
typecheckArgMod :: ArgMod (Maybe (Int, Int)) -> TEnv -> TypeCheckResult
typecheckArgMod x te = case x of
  AModVar _ -> typecheckfailure x
  AModVal _ -> typecheckfailure x
  AModInOut _ -> typecheckfailure x
typecheckTypeMod :: TypeMod (Maybe (Int, Int)) -> TEnv -> TypeCheckResult
typecheckTypeMod x te = case x of
  TModReadonly _ -> typecheckfailure x
typecheckExpr :: Expr (Maybe (Int, Int)) -> TEnv -> TypeCheckResult
typecheckExpr x te = case x of
  EVar _ ident -> typecheckfailure x
  ELitInt lineInfo integer -> Ok (te, Just (FullType lineInfo [] (Int lineInfo)))
  ELitTrue lineInfo -> Ok (te, Just (FullType lineInfo [] (Bool lineInfo)))
  ELitFalse lineInfo -> Ok (te, Just (FullType lineInfo [] (Bool lineInfo)))
  EString lineInfo string -> Ok (te, Just (FullType lineInfo [] (Str lineInfo)))
  ELitFloat lineInfo double -> Ok (te, Just (FullType lineInfo [] (Float lineInfo)))
  EEmpList _ -> typecheckfailure x
  Neg _ expr -> typecheckfailure x
  Not _ expr -> typecheckfailure x
  ECons _ expr1 expr2 -> typecheckfailure x
  EMul _ expr1 mulop expr2 -> typecheckfailure x
  EAdd _ expr1 addop expr2 -> typecheckfailure x
  ERel _ expr1 relop expr2 -> typecheckfailure x
  EAnd _ expr1 andop expr2 -> typecheckfailure x
  EOr _ expr1 orop expr2 -> typecheckfailure x
  EList _ exprs -> typecheckfailure x
  EArr _ exprs -> typecheckfailure x
  EArrSize _ expr -> typecheckfailure x
  EApp _ ident exprs -> typecheckfailure x
  EIf _ expr1 expr2 expr3 -> typecheckfailure x
  ELambda _ fulltype args stmt -> typecheckfailure x
  ERand _ expr -> typecheckfailure x
  ERandDist _ expr1 expr2 -> typecheckfailure x
  EProb _ stmt expr -> typecheckfailure x
  EProbSamp _ expr1 stmt expr2 -> typecheckfailure x
typecheckAddOp :: AddOp (Maybe (Int, Int)) -> TEnv -> TypeCheckResult
typecheckAddOp x te = case x of
  Plus _ -> Ok (te, Nothing)
  Minus _ -> Ok (te, Nothing)
typecheckMulOp :: MulOp (Maybe (Int, Int)) -> TEnv -> TypeCheckResult
typecheckMulOp x te = case x of
  Times _ -> Ok (te, Nothing)
  Div _ -> Ok (te, Nothing)
  Mod _ -> Ok (te, Nothing)
typecheckRelOp :: RelOp (Maybe (Int, Int)) -> TEnv -> TypeCheckResult
typecheckRelOp x te = case x of
  LTH _ -> Ok (te, Nothing)
  LE _ -> Ok (te, Nothing)
  GTH _ -> Ok (te, Nothing)
  GE _ -> Ok (te, Nothing)
  EQU _ -> Ok (te, Nothing)
  NE _ -> Ok (te, Nothing)
typecheckOrOp :: OrOp (Maybe (Int, Int)) -> TEnv -> TypeCheckResult
typecheckOrOp x te = case x of
  Or _ -> Ok (te, Nothing)
typecheckAndOp :: AndOp (Maybe (Int, Int)) -> TEnv -> TypeCheckResult
typecheckAndOp x te = case x of
  And _ -> Ok (te, Nothing)