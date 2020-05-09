module Tyche.TypeCheck where

import Tyche.Abs
import Tyche.ErrM
import Tyche.Types

typecheckIdent :: Ident -> TEnv -> TypeCheckResult
typecheckIdent x te = case x of
  Ident string -> Ok Nothing
typecheckProgram :: Show a => Program a -> TypeCheckResult
typecheckProgram x = case x of
  Program _ stmt -> typecheckStmt stmt (\v -> Nothing)
typecheckArg :: Show a => Arg a -> TEnv -> TypeCheckResult
typecheckArg x te = case x of
  Arg _ argmod fullident fulltype -> typecheckfailure x
typecheckFullIdent :: Show a => FullIdent a -> TEnv -> TypeCheckResult
typecheckFullIdent x te = case x of
  FullIdent _ ident -> typecheckfailure x
  AnonIdent _ -> typecheckfailure x
typecheckStmt :: Show a => Stmt a -> TEnv -> TypeCheckResult
typecheckStmt x te = case x of
  Skip _ -> typecheckfailure x
  Break _ -> typecheckfailure x
  Continue _ -> typecheckfailure x
  Ret _ expr -> typecheckfailure x
  VRet _ -> typecheckfailure x
  VarDef _ fullident fulltype expr -> typecheckfailure x
  Ass _ ident expr -> typecheckfailure x
  FnDef _ fullident fulltype args stmt -> typecheckfailure x
  Cond _ expr stmt -> typecheckfailure x
  CondElse _ expr stmt1 stmt2 -> typecheckfailure x
  While _ expr stmt -> typecheckfailure x
  ForList _ ident expr stmt -> typecheckfailure x
  ForRange _ ident expr1 expr2 stmt -> typecheckfailure x
  Composition _ stmt1 stmt2 -> typecheckfailure x
typecheckType :: Show a => Type a -> TEnv -> TypeCheckResult
typecheckType x te = case x of
  Int _ -> typecheckfailure x
  Str _ -> typecheckfailure x
  Bool _ -> typecheckfailure x
  Void _ -> typecheckfailure x
  Float _ -> typecheckfailure x
  List _ fulltype -> typecheckfailure x
  Array _ fulltype -> typecheckfailure x
  Fun _ argtypes fulltype -> typecheckfailure x
typecheckArgType :: Show a => ArgType a -> TEnv -> TypeCheckResult
typecheckArgType x te = case x of
  ArgType _ argmod type_ -> typecheckfailure x
typecheckFullType :: Show a => FullType a -> TEnv -> TypeCheckResult
typecheckFullType x te = case x of
  FullType _ typemods type_ -> typecheckfailure x
typecheckArgMod :: Show a => ArgMod a -> TEnv -> TypeCheckResult
typecheckArgMod x te = case x of
  AModVar _ -> typecheckfailure x
  AModVal _ -> typecheckfailure x
  AModInOut _ -> typecheckfailure x
typecheckTypeMod :: Show a => TypeMod a -> TEnv -> TypeCheckResult
typecheckTypeMod x te = case x of
  TModReadonly _ -> typecheckfailure x
typecheckExpr :: Show a => Expr a -> TEnv -> TypeCheckResult
typecheckExpr x te = case x of
  EVar _ ident -> typecheckfailure x
  ELitInt _ integer -> typecheckfailure x
  ELitTrue _ -> typecheckfailure x
  ELitFalse _ -> typecheckfailure x
  EString _ string -> typecheckfailure x
  ELitFloat _ double -> typecheckfailure x
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
typecheckAddOp :: Show a => AddOp a -> TEnv -> TypeCheckResult
typecheckAddOp x te = case x of
  Plus _ -> Ok Nothing
  Minus _ -> Ok Nothing
typecheckMulOp :: Show a => MulOp a -> TEnv -> TypeCheckResult
typecheckMulOp x te = case x of
  Times _ -> Ok Nothing
  Div _ -> Ok Nothing
  Mod _ -> Ok Nothing
typecheckRelOp :: Show a => RelOp a -> TEnv -> TypeCheckResult
typecheckRelOp x te = case x of
  LTH _ -> Ok Nothing
  LE _ -> Ok Nothing
  GTH _ -> Ok Nothing
  GE _ -> Ok Nothing
  EQU _ -> Ok Nothing
  NE _ -> Ok Nothing
typecheckOrOp :: Show a => OrOp a -> TEnv -> TypeCheckResult
typecheckOrOp x te = case x of
  Or _ -> Ok Nothing
typecheckAndOp :: Show a => AndOp a -> TEnv -> TypeCheckResult
typecheckAndOp x te = case x of
  And _ -> Ok Nothing