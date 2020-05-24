module Tyche.Trans where

import           Tyche.Abs
import           Tyche.Bool
import           Tyche.ErrM
import           Tyche.Helpers
import           Tyche.Numerical
import           Tyche.Print
import           Tyche.Types


transIdent :: Ident -> ()
transIdent x = case x of
  Ident string -> ()
transProgram :: Program LineInfo -> IO (Err ())
transProgram x = case x of
  Program lineinfo stmts -> do
    cont <- transStmts stmts (\v -> Nothing) (LEnv (\v -> Nothing)) (return (\venv -> return (\state -> return state)))
    (finalStore, finalError) <- cont ((0, \l -> NoVal), NoError [(Nothing, FullType Nothing [] (Fun Nothing [] voidT))])
    case finalError of
      NoError _ -> return (Ok ())
      otherwise -> return (Bad (show finalError))
transArg :: Arg LineInfo -> ()
transArg x = case x of
  Arg _ argmod ident fulltype -> ()
transStmts :: [Stmt LineInfo] -> VEnv -> LEnv -> IO ICont -> IO Cont
transStmts x venv (LEnv lenv) ioicont = case x of
  [] -> do
    icont <- ioicont
    icont venv
  stmt:stmts ->
    let
      afterioicont = (return (\aftervenv -> transStmts stmts aftervenv (LEnv lenv) ioicont))
    in
      transStmt stmt venv (LEnv lenv) afterioicont
transStmt :: Stmt LineInfo -> VEnv -> LEnv -> IO ICont -> IO Cont
transStmt x venv (LEnv lenv) ioicont = case x of
  Skip _ -> do
    icont <- ioicont
    icont venv
  Break _ -> do
    icont <- ioicont
    icont venv
  Continue _ -> do
    icont <- ioicont
    icont venv
  Ret _ expr -> do
    icont <- ioicont
    icont venv
  VarDef _ ident fulltype expr -> do
    icont <- ioicont
    icont venv
  Ass _ ident expr -> do
    icont <- ioicont
    icont venv
  FnDef _ ident fulltype args stmts -> do
    icont <- ioicont
    icont venv
  Cond _ expr stmts -> do
    icont <- ioicont
    icont venv
  CondElse _ expr stmts1 stmts2 -> do
    icont <- ioicont
    icont venv
  While _ expr stmts -> do
    icont <- ioicont
    icont venv
  ForList _ ident expr stmts -> do
    icont <- ioicont
    icont venv
  ForRange _ ident expr1 expr2 stmts -> do
    icont <- ioicont
    icont venv
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
transExpr :: Expr LineInfo -> VEnv -> LEnv -> ECont -> IO Cont
transExpr x venv (LEnv lenv) econt = case x of
  ELitVoid _                    -> econt NoVal
  EVar _ ident                  -> econt NoVal
  ELitInt _ integer             -> econt NoVal
  ELitTrue _                    -> econt NoVal
  ELitFalse _                   -> econt NoVal
  EString _ string              -> econt NoVal
  ELitFloat _ double            -> econt NoVal
  EEmpList _ fulltype           -> econt NoVal
  EApp _ expr exprs             -> econt NoVal
  Neg _ expr                    -> econt NoVal
  Not _ expr                    -> econt NoVal
  ECons _ expr1 expr2           -> econt NoVal
  EMul _ expr1 mulop expr2      -> econt NoVal
  EAdd _ expr1 addop expr2      -> econt NoVal
  ERel _ expr1 relop expr2      -> econt NoVal
  EAnd _ expr1 andop expr2      -> econt NoVal
  EOr _ expr1 orop expr2        -> econt NoVal
  EList _ exprs                 -> econt NoVal
  EArr _ exprs                  -> econt NoVal
  EArrSize _ fulltype expr      -> econt NoVal
  EArrApp _ expr1 expr2         -> econt NoVal
  EIf _ expr1 expr2 expr3       -> econt NoVal
  ELambda _ fulltype args stmts -> econt NoVal
  ERand _ expr                  -> econt NoVal
  ERandDist _ expr1 expr2       -> econt NoVal
  EProbSamp _ expr1 stmts expr2 -> econt NoVal
transAddOp :: AddOp LineInfo -> Val -> Val -> (Val, Error)
transAddOp x v1 v2 = case x of
  Plus _  -> (addNumericals v1 v2, NoError [])
  Minus _ -> (substractNumericals v1 v2, NoError [])
transMulOp :: MulOp LineInfo -> Val -> Val -> (Val, Error)
transMulOp x v1 v2 = case x of
  Times _ -> (multiplyNumericals v1 v2, NoError [])
  Div _   -> case v2 of
    FloatVal 0 -> (NoVal, DivisionBy0 [])
    IntVal 0   -> (NoVal, DivisionBy0 [])
    otherwise  -> (divideNumericals v1 v2, NoError [])
  Mod _   -> case v2 of
    FloatVal 0 -> (NoVal, DivisionBy0 [])
    IntVal 0   -> (NoVal, DivisionBy0 [])
    otherwise  -> (modNumericals v1 v2, NoError [])
transRelOp :: RelOp LineInfo -> Val -> Val -> (Val, Error)
transRelOp x v1 v2 = case x of
  LTH _ -> (lthNumericals v1 v2, NoError [])
  LE _  -> (leNumericals v1 v2, NoError [])
  GTH _ -> (gthNumericals v1 v2, NoError [])
  GE _  -> (geNumericals v1 v2, NoError [])
  EQU _ -> (equNumericals v1 v2, NoError [])
  NE _  -> (neNumericals v1 v2, NoError [])
transOrOp :: OrOp LineInfo -> Val -> Val -> (Val, Error)
transOrOp x v1 v2 = case x of
  Or _ -> case (v1, v2) of
    (BoolVal x1, BoolVal x2) -> (BoolVal (x1 || x2), NoError [])
transAndOp :: AndOp LineInfo -> Val -> Val -> (Val, Error)
transAndOp x v1 v2 = case x of
  And _ -> case (v1, v2) of
    (BoolVal x1, BoolVal x2) -> (BoolVal (x1 && x2), NoError [])
