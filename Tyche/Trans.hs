module Tyche.Trans where

import Tyche.Abs
import Tyche.Types
import Tyche.ErrM
import Tyche.Print


transIdent :: Ident -> ()
transIdent x =
  case x of
    Ident string ->
      ()
transProgram :: Program (Maybe (Int, Int)) -> IO (Err ())
transProgram x =
  case x of
    Program _ stmt -> do
      cont <- (transStmt stmt (\v -> Nothing) (\v -> Nothing) (LEnv (\v -> Nothing)) (return (\state -> return state)))
      (finalState, finalError) <- cont ((0, \l -> NoVal), NoError)
      printState (finalState, finalError)
      case finalError of
        NoError -> do
          return (Ok ())
        otherwise -> do
          return (Bad (show finalError))
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
transStmt :: Stmt (Maybe (Int, Int)) -> TEnv -> VEnv -> LEnv -> IO (Cont) -> IO (Cont)
transStmt x tenv venv (LEnv lenv) cont =
  case x of
    Skip lineInfo -> 
      cont
    Break lineInfo ->
      case (lenv LBreak) of
        Nothing ->
          return (\((next, store), err) -> return ((next, store), BreakError))
        Just c1 ->
          return (c1)
    Continue lineInfo ->
      cont
    Ret lineInfo expr -> do
      transExpr expr tenv venv (LEnv lenv) (\val -> (cont))
    VRet lineInfo -> do
      cont
    VarDef lineInfo fullident fulltype expr -> do
      cont
    Ass lineInfo ident expr -> do
      cont
    FnDef lineInfo fullident fulltype args stmt -> do
      cont
    Cond lineInfo expr stmt -> do
      cont
    CondElse lineInfo expr stmt1 stmt2 -> do
      cont
    While lineInfo expr stmt -> do
      transExpr expr tenv venv (LEnv lenv) (\val ->
          (case val of
            BoolVal bval ->
              if bval then
                transStmt stmt tenv venv (LEnv lenv) (transStmt x tenv venv (LEnv lenv) cont)
              else
                cont
            otherwise ->
              return (\((next, store), err) -> return ((next, store), TypeError))))
    ForList lineInfo ident expr stmt -> do
      cont
    ForRange lineInfo ident expr1 expr2 stmt -> do
      cont
    Composition lineInfo stmt1 stmt2 -> do
      transStmt stmt1 tenv venv (LEnv lenv) (transStmt stmt2 tenv venv (LEnv lenv) cont)
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
transExpr x tenv venv lenv econt = do
  case x of
    EVar lineInfo ident ->
      case venv ident of
        Nothing ->
          return (\((next, store), err) -> return ((next, store), TypeError))
        Just loc ->
          return (\((next, store), err) -> do
            nc <- (econt (store loc))
            nc ((next, store), err))
    ELitInt _ integer ->
      econt (IntVal integer)
    ELitTrue _ ->
      econt (BoolVal True)
    ELitFalse _ ->
      econt (BoolVal False)
    EString _ string ->
      econt (StringVal string)
    ELitFloat _ double ->
      econt (FloatVal double)
    EEmpList _ fulltype ->
      return (\((next, store), err) -> 
          let
            (l, store'@(next', s')) = newLoc (next, store)
          in
            return (saveInStore store' l (ListVal (l, NoVal, Nothing)), err))
    Neg _ expr ->
      econt (BoolVal True)
    Not _ expr ->
      econt (BoolVal True)
    ECons _ expr1 expr2 ->
      econt (BoolVal True)
    EMul _ expr1 mulop expr2 ->
      econt (BoolVal True)
    EAdd _ expr1 addop expr2 ->
      econt (BoolVal True)
    ERel _ expr1 relop expr2 ->
      econt (BoolVal True)
    EAnd _ expr1 andop expr2 ->
      econt (BoolVal True)
    EOr _ expr1 orop expr2 ->
      econt (BoolVal True)
    EList _ exprs ->
      econt (BoolVal True)
    EArr _ exprs ->
      econt (BoolVal True)
    EArrSize _ fulltype expr ->
      econt (BoolVal True)
    EApp _ expr exprs ->
      econt (BoolVal True)
    EArrApp _ expr exprs ->
      econt (BoolVal True)
    EIf _ expr1 expr2 expr3 ->
      econt (BoolVal True)
    ELambda _ fulltype args stmt ->
      econt (BoolVal True)
    ERand _ expr ->
      econt (BoolVal True)
    ERandDist _ expr1 expr2 ->
      econt (BoolVal True)
    EProb _ stmt expr ->
      econt (BoolVal True)
    EProbSamp _ expr1 stmt expr2 ->
      econt (BoolVal True)
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

