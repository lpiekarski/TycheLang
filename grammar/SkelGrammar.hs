module SkelGrammar where

-- Haskell module generated by the BNF converter

import AbsGrammar
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident string -> failure x
transProgram :: Show a => Program a -> Result
transProgram x = case x of
  Program _ stmt -> failure x
transArg :: Show a => Arg a -> Result
transArg x = case x of
  Arg _ argmod fullident fulltype -> failure x
transFullIdent :: Show a => FullIdent a -> Result
transFullIdent x = case x of
  FullIdent _ ident -> failure x
  AnonIdent _ -> failure x
transStmt :: Show a => Stmt a -> Result
transStmt x = case x of
  Skip _ -> failure x
  Break _ -> failure x
  Continue _ -> failure x
  Ret _ expr -> failure x
  VRet _ -> failure x
  VarDef _ fullident fulltype expr -> failure x
  Ass _ ident expr -> failure x
  FnDef _ fullident fulltype args stmt -> failure x
  Cond _ expr stmt -> failure x
  CondElse _ expr stmt1 stmt2 -> failure x
  While _ expr stmt -> failure x
  ForList _ ident expr stmt -> failure x
  ForRange _ ident expr1 expr2 stmt -> failure x
  Composition _ stmt1 stmt2 -> failure x
transType :: Show a => Type a -> Result
transType x = case x of
  Int _ -> failure x
  Str _ -> failure x
  Bool _ -> failure x
  Void _ -> failure x
  Float _ -> failure x
  List _ fulltype -> failure x
  Array _ fulltype -> failure x
  Fun _ argtypes fulltype -> failure x
transArgType :: Show a => ArgType a -> Result
transArgType x = case x of
  ArgType _ argmod type_ -> failure x
transFullType :: Show a => FullType a -> Result
transFullType x = case x of
  FullType _ typemods type_ -> failure x
transArgMod :: Show a => ArgMod a -> Result
transArgMod x = case x of
  AModVar _ -> failure x
  AModVal _ -> failure x
  AModInOut _ -> failure x
transTypeMod :: Show a => TypeMod a -> Result
transTypeMod x = case x of
  TModReadonly _ -> failure x
transExpr :: Show a => Expr a -> Result
transExpr x = case x of
  EVar _ ident -> failure x
  ELitInt _ integer -> failure x
  ELitTrue _ -> failure x
  ELitFalse _ -> failure x
  EString _ string -> failure x
  ELitFloat _ double -> failure x
  EEmpList _ -> failure x
  Neg _ expr -> failure x
  Not _ expr -> failure x
  ECons _ expr1 expr2 -> failure x
  EMul _ expr1 mulop expr2 -> failure x
  EAdd _ expr1 addop expr2 -> failure x
  ERel _ expr1 relop expr2 -> failure x
  EAnd _ expr1 andop expr2 -> failure x
  EOr _ expr1 orop expr2 -> failure x
  EList _ exprs -> failure x
  EArr _ exprs -> failure x
  EArrSize _ expr -> failure x
  EApp _ ident exprs -> failure x
  EIf _ expr1 expr2 expr3 -> failure x
  ELambda _ fulltype args stmt -> failure x
  ERand _ expr -> failure x
  ERandDist _ expr1 expr2 -> failure x
  EProb _ stmt expr -> failure x
  EProbSamp _ expr1 stmt expr2 -> failure x
transAddOp :: Show a => AddOp a -> Result
transAddOp x = case x of
  Plus _ -> failure x
  Minus _ -> failure x
transMulOp :: Show a => MulOp a -> Result
transMulOp x = case x of
  Times _ -> failure x
  Div _ -> failure x
  Mod _ -> failure x
transRelOp :: Show a => RelOp a -> Result
transRelOp x = case x of
  LTH _ -> failure x
  LE _ -> failure x
  GTH _ -> failure x
  GE _ -> failure x
  EQU _ -> failure x
  NE _ -> failure x
transOrOp :: Show a => OrOp a -> Result
transOrOp x = case x of
  Or _ -> failure x
transAndOp :: Show a => AndOp a -> Result
transAndOp x = case x of
  And _ -> failure x

