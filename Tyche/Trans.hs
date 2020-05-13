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
      cont <- (transStmt stmt (\v -> Nothing) (\v -> Nothing) (LEnv (\v -> Nothing)) (return (\tenv -> \venv -> return (\state -> return state))))
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
transStmt :: Stmt (Maybe (Int, Int)) -> TEnv -> VEnv -> LEnv -> IO (ICont) -> IO (Cont)
transStmt x tenv venv (LEnv lenv) icont =
  case x of
    Skip lineInfo -> do
      ic <- icont
      ic tenv venv
    Break lineInfo ->
      case (lenv LBreak) of
        Nothing ->
          return (\((next, store), err) -> return ((next, store), BreakError))
        Just c1 ->
          c1
    Continue lineInfo ->
      case (lenv LContinue) of
        Nothing ->
          return (\((next, store), err) -> return ((next, store), ContinueError))
        Just c1 ->
          c1
    Ret lineInfo expr -> do
      ic <- icont
      transExpr expr tenv venv (LEnv lenv) (\valtype -> \val -> (ic tenv venv))
    VRet lineInfo ->
      case (lenv LReturn) of
        Nothing -> do
          ic <- icont
          ic tenv venv
        Just c -> do
          c
    VarDef lineInfo fullident fulltype expr ->
      case fullident of
        AnonIdent _ ->
          transExpr expr tenv venv (LEnv lenv) (\valtype -> \val -> return (\(store, err) -> do
          let (l, store') = newLoc store
          ic <- icont
          c <- (ic tenv venv)
          c (saveInStore store' l val, err)))
        FullIdent _ ident ->
          transExpr expr tenv venv (LEnv lenv) (\valtype -> \val -> return (\(store, err) -> do
          let (l, store') = newLoc store
          ic <- icont
          let venv' = extendFunc venv ident (Just l)
          let tenv' = extendFunc tenv ident (Just fulltype)
          c <- (ic tenv' venv')
          c (saveInStore store' l val, err)))
    Ass lineInfo ident expr -> do
      ic <- icont
      ic tenv venv
    FnDef lineInfo fullident fulltype args stmt -> do
      ic <- icont
      ic tenv venv
    Cond lineInfo expr stmt -> do
      ic <- icont
      ic tenv venv
    CondElse lineInfo expr stmt1 stmt2 -> do
      ic <- icont
      ic tenv venv
    While lineInfo expr stmt ->
      transExpr expr tenv venv (LEnv lenv) (\fulltype -> \val ->
          (case val of
            BoolVal bval ->
              if bval then
                let
                  c1 = transStmt x tenv venv (LEnv lenv) icont
                  ic1 = (return (\tenv' -> \venv' -> c1))
                in do
                  ic <- icont
                  transStmt stmt tenv venv (addContinueLabel (addBreakLabel (LEnv lenv) (ic tenv venv)) c1) ic1
              else do
                ic <- icont
                ic tenv venv
            otherwise ->
              return (\((next, store), err) -> return ((next, store), TypeError))))
    ForList lineInfo ident expr stmt -> do
      ic <- icont
      ic tenv venv
    ForRange lineInfo ident expr1 expr2 stmt -> do
      ic <- icont
      ic tenv venv
    Composition lineInfo stmt1 stmt2 ->
      transStmt stmt1 tenv venv (LEnv lenv) (return (\tenv' -> \venv' -> (transStmt stmt2 tenv' venv' (LEnv lenv) icont)))
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
transExpr x tenv venv (LEnv lenv) econt = do
  case x of
    EVar lineInfo ident ->
      case venv ident of
        Nothing ->
          return (\((next, store), err) -> return ((next, store), TypeError))
        Just loc ->
          return (\((next, store), err) -> 
            case tenv ident of
              Nothing ->
                return ((next, store), TypeError)
              Just fulltype -> do
                nc <- (econt fulltype (store loc))
                nc ((next, store), err))
    ELitInt _ integer ->
      econt (readonlyIntT) (IntVal integer)
    ELitTrue _ ->
      econt (readonlyBoolT) (BoolVal True)
    ELitFalse _ ->
      econt (readonlyBoolT) (BoolVal False)
    EString _ string ->
      econt (readonlyStringT) (StringVal string)
    ELitFloat _ double ->
      econt (readonlyFloatT) (FloatVal double)
    EEmpList _ fulltype ->
      econt (readonlyListT fulltype) (ListVal [])
    Neg _ expr ->
      transExpr expr tenv venv (LEnv lenv) (\valtype -> \val -> econt valtype (negateNum val))
    Not _ expr ->
      transExpr expr tenv venv (LEnv lenv) (\valtype -> \val -> econt valtype (negateBool val))
    ECons _ expr1 expr2 ->
      transExpr expr1 tenv venv (LEnv lenv) (\eltype -> \el -> transExpr expr2 tenv venv (LEnv lenv) (\listtype -> \listval ->
        case listval of
          ListVal list -> 
            econt (readonlyListT listtype) (ListVal (el:list))
          otherwise ->
            return (\(state, err) -> return (state, TypeError))
        ))
    EMul _ expr1 mulop expr2 ->
      transExpr expr1 tenv venv (LEnv lenv) (\type1 -> \val1 ->
        transExpr expr2 tenv venv (LEnv lenv) (\type2 -> \val2 ->
          case transMulOp mulop val1 val2 of
            (restype, resval, NoError) ->
              econt restype resval
            (_, _, err') ->
              return (\(state, err) -> return (state, err'))))
    EAdd _ expr1 addop expr2 ->
      transExpr expr1 tenv venv (LEnv lenv) (\type1 -> \val1 ->
        transExpr expr2 tenv venv (LEnv lenv) (\type2 -> \val2 ->
          case transAddOp addop val1 val2 of
            (restype, resval, NoError) ->
              econt restype resval
            (_, _, err') ->
              return (\(state, err) -> return (state, err'))))
    ERel _ expr1 relop expr2 ->
      transExpr expr1 tenv venv (LEnv lenv) (\type1 -> \val1 ->
        transExpr expr2 tenv venv (LEnv lenv) (\type2 -> \val2 ->
          case transRelOp relop val1 val2 of
            (restype, resval, NoError) ->
              econt restype resval
            (_, _, err') ->
              return (\(state, err) -> return (state, err'))))
    EAnd _ expr1 andop expr2 ->
      transExpr expr1 tenv venv (LEnv lenv) (\type1 -> \val1 ->
        transExpr expr2 tenv venv (LEnv lenv) (\type2 -> \val2 ->
          case transAndOp andop val1 val2 of
            (restype, resval, NoError) ->
              econt restype resval
            (_, _, err') ->
              return (\(state, err) -> return (state, err'))))
    EOr _ expr1 orop expr2 ->
      transExpr expr1 tenv venv (LEnv lenv) (\type1 -> \val1 ->
        transExpr expr2 tenv venv (LEnv lenv) (\type2 -> \val2 ->
          case transOrOp orop val1 val2 of
            (restype, resval, NoError) ->
              econt restype resval
            (_, _, err') ->
              return (\(state, err) -> return (state, err'))))
    EList _ exprs ->
      econt (readonlyBoolT) (BoolVal True)
    EArr _ exprs ->
      econt (readonlyBoolT) (BoolVal True)
    EArrSize _ fulltype expr ->
      econt (readonlyBoolT) (BoolVal True)
    EApp _ expr exprs ->
      econt (readonlyBoolT) (BoolVal True)
    EArrApp _ expr exprs ->
      econt (readonlyBoolT) (BoolVal True)
    EIf _ expr1 expr2 expr3 ->
      econt (readonlyBoolT) (BoolVal True)
    ELambda _ fulltype args stmt ->
      econt (readonlyBoolT) (BoolVal True)
    ERand _ expr ->
      econt (readonlyBoolT) (BoolVal True)
    ERandDist _ expr1 expr2 ->
      econt (readonlyBoolT) (BoolVal True)
    EProb _ stmt expr ->
      econt (readonlyBoolT) (BoolVal True)
    EProbSamp _ expr1 stmt expr2 ->
      econt (readonlyBoolT) (BoolVal True)
transAddOp :: AddOp (Maybe (Int, Int)) -> Val -> Val -> (FullType (Maybe (Int, Int)), Val, Error)
transAddOp x v1 v2 =
  case x of
    Plus _ ->
      case (v1, v2) of
        (IntVal x1, IntVal x2) ->
          (readonlyIntT, IntVal (x1 + x2), NoError)
        (FloatVal x1, FloatVal x2) ->
          (readonlyFloatT, FloatVal (x1 + x2), NoError)
        otherwise ->
          (readonlyVoidT, NoVal, TypeError)
    Minus _ ->
      case (v1, v2) of
        (IntVal x1, IntVal x2) ->
          (readonlyIntT, IntVal (x1 - x2), NoError)
        (FloatVal x1, FloatVal x2) ->
          (readonlyFloatT, FloatVal (x1 - x2), NoError)
        otherwise ->
          (readonlyVoidT, NoVal, TypeError)
transMulOp :: MulOp (Maybe (Int, Int)) -> Val -> Val -> (FullType (Maybe (Int, Int)), Val, Error)
transMulOp x v1 v2 =
  case x of
    Times _ ->
      case (v1, v2) of
        (IntVal x1, IntVal x2) ->
          (readonlyIntT, IntVal (x1 * x2), NoError)
        (FloatVal x1, FloatVal x2) ->
          (readonlyFloatT, FloatVal (x1 * x2), NoError)
        otherwise ->
          (readonlyVoidT, NoVal, TypeError)
    Div lineInfo ->
      case (v1, v2) of
        (IntVal x1, IntVal x2) ->
          if x2 == 0 then
            (readonlyVoidT, NoVal, DivisionBy0)
          else
            (readonlyIntT, IntVal (x1 `div` x2), NoError)
        (FloatVal x1, FloatVal x2) ->
          if x2 == 0 then
            (readonlyVoidT, NoVal, DivisionBy0)
          else
            (readonlyFloatT, FloatVal (x1 / x2), NoError)
        otherwise ->
          (readonlyVoidT, NoVal, TypeError)
    Mod _ ->
      case (v1, v2) of
        (IntVal x1, IntVal x2) ->
          if x2 == 0 then
            (readonlyVoidT, NoVal, DivisionBy0)
          else
            (readonlyIntT, IntVal (x1 `mod` x2), NoError)
        otherwise ->
          (readonlyVoidT, NoVal, TypeError)
transRelOp :: RelOp (Maybe (Int, Int)) -> Val -> Val -> (FullType (Maybe (Int, Int)), Val, Error)
transRelOp x v1 v2 =
  case x of
    LTH _ ->
      case (v1, v2) of
        (IntVal x1, IntVal x2) ->
          (readonlyBoolT, BoolVal (x1 < x2), NoError)
        otherwise ->
          (readonlyVoidT, NoVal, TypeError)
    LE _ ->
      case (v1, v2) of
        (IntVal x1, IntVal x2) ->
          (readonlyBoolT, BoolVal (x1 <= x2), NoError)
        otherwise ->
          (readonlyVoidT, NoVal, TypeError)
    GTH _ ->
      case (v1, v2) of
        (IntVal x1, IntVal x2) ->
          (readonlyBoolT, BoolVal (x1 > x2), NoError)
        otherwise ->
          (readonlyVoidT, NoVal, TypeError)
    GE _ ->
      case (v1, v2) of
        (IntVal x1, IntVal x2) ->
          (readonlyBoolT, BoolVal (x1 >= x2), NoError)
        otherwise ->
          (readonlyVoidT, NoVal, TypeError)
    EQU _ ->
      case (v1, v2) of
        (IntVal x1, IntVal x2) ->
          (readonlyBoolT, BoolVal (x1 == x2), NoError)
        otherwise ->
          (readonlyVoidT, NoVal, TypeError)
    NE _ ->
      case (v1, v2) of
        (IntVal x1, IntVal x2) ->
          (readonlyBoolT, BoolVal (x1 /= x2), NoError)
        otherwise ->
          (readonlyVoidT, NoVal, TypeError)
transOrOp :: OrOp (Maybe (Int, Int)) -> Val -> Val -> (FullType (Maybe (Int, Int)), Val, Error)
transOrOp x v1 v2 =
  case x of
    Or _ ->
      case (v1, v2) of
        (BoolVal x1, BoolVal x2) ->
          (readonlyBoolT, BoolVal (x1 || x2), NoError)
        otherwise ->
          (readonlyVoidT, NoVal, TypeError)
transAndOp :: AndOp (Maybe (Int, Int)) -> Val -> Val -> (FullType (Maybe (Int, Int)), Val, Error)
transAndOp x v1 v2 = 
  case x of
    And _ ->
      case (v1, v2) of
        (BoolVal x1, BoolVal x2) ->
          (readonlyBoolT, BoolVal (x1 && x2), NoError)
        otherwise ->
          (readonlyVoidT, NoVal, TypeError)

