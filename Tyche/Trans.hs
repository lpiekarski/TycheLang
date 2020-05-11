module Tyche.Trans where

import Tyche.Abs
import Tyche.Types
import Tyche.ErrM


transIdent :: Ident -> ()
transIdent x =
  case x of
    Ident string ->
      ()
transProgram :: Program (Maybe (Int, Int)) -> IO (Err ())
transProgram x =
  case x of
    Program _ stmt -> do
      res <- transStmt stmt (\v -> Nothing) (\v -> Nothing) (LEnv (\v -> Nothing)) (Ok (\state -> state))
      case res of
        Bad str -> do
          return (Bad str)
        Ok _ -> do
          return (Ok ())
transArg :: Arg (Maybe (Int, Int)) -> ()
transArg x =
  case x of
    Arg _ argmod fullident fulltype ->
      ()
transFullIdent :: FullIdent (Maybe (Int, Int)) -> ()
transFullIdent x =
  case x of
    FullIdent _ ident ->
      ()
    AnonIdent _ ->
      ()
transStmt :: Stmt (Maybe (Int, Int)) -> TEnv -> VEnv -> LEnv -> Cont -> IO (Cont)
transStmt x tenv venv lenv cont =
  case x of
    Skip lineInfo -> do
      return (cont)
    Break lineInfo -> do
      return (cont)
    Continue lineInfo -> do
      return (cont)
    Ret lineInfo expr -> do
      return (cont)
    VRet lineInfo -> do
      return (cont)
    VarDef lineInfo fullident fulltype expr -> do
      return (cont)
    Ass lineInfo ident expr -> do
      return (cont)
    FnDef lineInfo fullident fulltype args stmt -> do
      return (cont)
    Cond lineInfo expr stmt -> do
      return (cont)
    CondElse lineInfo expr stmt1 stmt2 -> do
      return (cont)
    While lineInfo expr stmt -> do
      return (cont)
    ForList lineInfo ident expr stmt -> do
      return (cont)
    ForRange lineInfo ident expr1 expr2 stmt -> do
      return (cont)
    Composition lineInfo stmt1 stmt2 -> do
      s2 <- transStmt stmt2 tenv venv lenv cont
      s1 <- transStmt stmt1 tenv venv lenv s2
      return (s1)
transType :: Type (Maybe (Int, Int)) -> ()
transType x =
  case x of
    Int _ ->
      ()
    Str _ ->
      ()
    Bool _ ->
      ()
    Void _ ->
      ()
    Float _ ->
      ()
    List _ fulltype ->
      ()
    Array _ fulltype ->
      ()
    Fun _ argtypes fulltype ->
      ()
transArgType :: ArgType (Maybe (Int, Int)) -> ()
transArgType x =
  case x of
    ArgType _ argmod type_ ->
      ()
transFullType :: FullType (Maybe (Int, Int)) -> ()
transFullType x =
  case x of
    FullType _ typemods type_ ->
      ()
transArgMod :: ArgMod a -> ()
transArgMod x =
  case x of
    AModVar _ ->
      ()
    AModVal _ ->
      ()
    AModInOut _ ->
      ()
transTypeMod :: TypeMod a -> ()
transTypeMod x =
  case x of
    TModReadonly _ ->
      ()
transExpr :: Expr (Maybe (Int, Int)) -> TEnv -> VEnv -> LEnv -> ECont -> IO (Cont)
transExpr x tenv venv lenv econt =
  case x of
    EVar _ ident -> do
      return (econt (BoolVal True))
    ELitInt _ integer -> do
      return (econt (BoolVal True))
    ELitTrue _ -> do
      return (econt (BoolVal True))
    ELitFalse _ -> do
      return (econt (BoolVal True))
    EString _ string -> do
      return (econt (BoolVal True))
    ELitFloat _ double -> do
      return (econt (BoolVal True))
    EEmpList _ fulltype -> do
      return (econt (BoolVal True))
    Neg _ expr -> do
      return (econt (BoolVal True))
    Not _ expr -> do
      return (econt (BoolVal True))
    ECons _ expr1 expr2 -> do
      return (econt (BoolVal True))
    EMul _ expr1 mulop expr2 -> do
      return (econt (BoolVal True))
    EAdd _ expr1 addop expr2 -> do
      return (econt (BoolVal True))
    ERel _ expr1 relop expr2 -> do
      return (econt (BoolVal True))
    EAnd _ expr1 andop expr2 -> do
      return (econt (BoolVal True))
    EOr _ expr1 orop expr2 -> do
      return (econt (BoolVal True))
    EList _ exprs -> do
      return (econt (BoolVal True))
    EArr _ exprs -> do
      return (econt (BoolVal True))
    EArrSize _ fulltype expr -> do
      return (econt (BoolVal True))
    EApp _ expr exprs -> do
      return (econt (BoolVal True))
    EArrApp _ expr exprs -> do
      return (econt (BoolVal True))
    EIf _ expr1 expr2 expr3 -> do
      return (econt (BoolVal True))
    ELambda _ fulltype args stmt -> do
      return (econt (BoolVal True))
    ERand _ expr -> do
      return (econt (BoolVal True))
    ERandDist _ expr1 expr2 -> do
      return (econt (BoolVal True))
    EProb _ stmt expr -> do
      return (econt (BoolVal True))
    EProbSamp _ expr1 stmt expr2 -> do
      return (econt (BoolVal True))
transAddOp :: AddOp (Maybe (Int, Int)) -> ()
transAddOp x =
  case x of
    Plus _ ->
      ()
    Minus _ ->
      ()
transMulOp :: MulOp (Maybe (Int, Int)) -> ()
transMulOp x =
  case x of
    Times _ ->
      ()
    Div lineInfo ->
      ()
    Mod _ ->
      ()
transRelOp :: RelOp (Maybe (Int, Int)) -> ()
transRelOp x =
  case x of
    LTH _ ->
      ()
    LE _ ->
      ()
    GTH _ ->
      ()
    GE _ ->
      ()
    EQU _ ->
      ()
    NE _ ->
      ()
transOrOp :: OrOp (Maybe (Int, Int)) -> ()
transOrOp x =
  case x of
    Or _ ->
      ()
transAndOp :: AndOp (Maybe (Int, Int)) -> ()
transAndOp x = 
  case x of
    And _ ->
      ()

