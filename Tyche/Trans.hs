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
    Program lineInfo stmt -> do
      cont <- (transStmt stmt (\v -> Nothing) (\v -> Nothing) (LEnv (\v -> Nothing)) (return (\tenv -> \venv -> return (\state -> return state))))
      (finalState, finalError) <- cont ((0, \l -> NoVal), NoError lineInfo)
      printState (finalState, finalError)
      case finalError of
        NoError _ -> do
          return (Ok ())
        otherwise -> do
          return (Bad (show finalError))
transArg :: Arg (Maybe (Int, Int)) -> ()
transArg x =
  case x of
    Arg lineInfo argmod fullident fulltype ->
      ()
transFullIdent :: FullIdent (Maybe (Int, Int)) -> ()
transFullIdent x =
  case x of
    FullIdent lineInfo ident ->
      ()
    AnonIdent lineInfo ->
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
          return (\((next, store), err) -> return ((next, store), BreakError lineInfo))
        Just iec -> do
          ec <- iec
          ec readonlyVoidT NoVal
    Continue lineInfo ->
      case (lenv LContinue) of
        Nothing ->
          return (\((next, store), err) -> return ((next, store), ContinueError lineInfo))
        Just iec -> do
          ec <- iec
          ec readonlyVoidT NoVal
    Ret lineInfo expr ->
      case (lenv LReturn) of
        Nothing ->
          return (\(state, err) -> return (state, ReturnError lineInfo))
        Just iec -> do
          ec <- iec
          transExpr expr tenv venv (LEnv lenv) ec
    VRet lineInfo -> 
      case (lenv LReturn) of
        Nothing -> do
          return (\(state, err) -> return (state, ReturnError lineInfo))
        Just iec -> do
          ec <- iec
          ec readonlyVoidT NoVal
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
    Ass lineInfo ident expr ->
      transExpr expr tenv venv (LEnv lenv) (\valtype -> \val -> return (\(store, err) ->
        case venv ident of
          Nothing -> do
            let (l, store') = newLoc store
            ic <- icont
            let venv' = extendFunc venv ident (Just l)
            let tenv' = extendFunc tenv ident (Just valtype)
            c <- (ic tenv' venv')
            c (saveInStore store' l val, err)
          Just l -> do
            ic <- icont
            c <- (ic tenv venv)
            c (saveInStore store l val, err)))
    FnDef lineInfo fullident fulltype args stmt ->
      case fullident of
        AnonIdent _ -> do
          ic <- icont
          c <- ic tenv venv
          return (\(state, err) -> do
            let (l, state') = newLoc state
            c (saveInStore state' l (FuncVal (\fts -> \as -> \funcioicont -> do
              let
                go [] [] tv vv (LEnv lv) st =
                  (tv, vv, (LEnv lv), st)
                go (argtype:argtypes) (argument:arguments) tv vv (LEnv lv) st =
                  case argument of
                    Variable loc idnt ->
                      go argtypes arguments (extendFunc tv idnt (Just argtype)) (extendFunc vv idnt (Just loc)) (LEnv lv) st
                    Value val idnt -> do
                      let (l'', st'') = newLoc st
                      go argtypes arguments (extendFunc tv idnt (Just argtype)) (extendFunc vv idnt (Just l'')) (LEnv lv) (saveInStore st'' l'' val)
                    otherwise ->
                      go argtypes arguments tv vv (LEnv lv) st --Inout i idnt ->
              let (tenv', venv', (LEnv lenv'), state'') = go (argsToFullTypes args) (argsToArgVals args venv (state', err)) tenv venv (LEnv lenv) state'
              funcicont <- funcioicont
              funccont <- funcicont tenv' venv'
              return (\(s, e) -> funccont (state'', NoError lineInfo)))), err))
        FullIdent _ ident -> do
          ic <- icont
          c <- ic tenv venv
          return (\(state, err) -> do
            let (l, state') = newLoc state
            let venv = extendFunc venv ident (Just l)
            --let tenv = extendFunc tenv ident (Just fulltype)
            c (saveInStore state' l (FuncVal (\fts -> \as -> \funcioicont -> do
              let
                go [] [] tv vv (LEnv lv) st =
                  (tv, vv, (LEnv lv), st)
                go (argtype:argtypes) (argument:arguments) tv vv (LEnv lv) st =
                  case argument of
                    Variable loc idnt ->
                      go argtypes arguments (extendFunc tv idnt (Just argtype)) (extendFunc vv idnt (Just loc)) (LEnv lv) st
                    Value val idnt -> do
                      let (l'', st'') = newLoc st
                      go argtypes arguments (extendFunc tv idnt (Just argtype)) (extendFunc vv idnt (Just l'')) (LEnv lv) (saveInStore st'' l'' val)
                    otherwise ->
                      go argtypes arguments tv vv (LEnv lv) st --Inout i idnt ->
              let (tenv', venv', (LEnv lenv'), state'') = go (argsToFullTypes args) (argsToArgVals args venv (state', err)) tenv venv (LEnv lenv) state'
              funcicont <- funcioicont
              funccont <- funcicont tenv' venv'
              return (\(s, e) -> funccont (state'', NoError lineInfo)))), err))
    Cond lineInfo expr stmt -> do
      transExpr expr tenv venv (LEnv lenv) (\type1 -> \val1 -> 
        (case val1 of
          BoolVal bval ->
            if bval then do
              transStmt stmt tenv venv (LEnv lenv) icont
            else do
              ic <- icont
              ic tenv venv
          otherwise ->
            return (\(state, err) -> return (state, TypeError lineInfo))
          ))
    CondElse lineInfo expr stmt1 stmt2 -> do
      transExpr expr tenv venv (LEnv lenv) (\type1 -> \val1 -> 
        (case val1 of
          BoolVal bval ->
            if bval then do
              transStmt stmt1 tenv venv (LEnv lenv) icont
            else do
              transStmt stmt2 tenv venv (LEnv lenv) icont
          otherwise ->
            return (\(state, err) -> return (state, TypeError lineInfo))
          ))
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
                  transStmt stmt tenv venv (addContinueLabel (addBreakLabel (LEnv lenv) (return (\valtype -> \val -> ic tenv venv))) (return (\valtype -> \val -> c1))) ic1
              else do
                ic <- icont
                ic tenv venv
            otherwise ->
              return (\((next, store), err) -> return ((next, store), TypeError lineInfo))))
    ForList lineInfo ident expr stmt -> do
      transExpr expr tenv venv (LEnv lenv) (\valtype -> \val ->
        case val of
          ListVal list ->
            return (\(state, err) -> do
              let (l, state') = newLoc state
              ic <- icont
              let venv' = extendFunc venv ident (Just l)
              case listElementType valtype of
                Nothing ->
                  return (state, TypeError lineInfo)
                Just eltype -> do
                  let tenv' = extendFunc tenv ident (Just (addReadonly eltype))
                  --let lenv' = addBreakLabel (LEnv lenv) (return (\valtype -> \val -> ic tenv venv))
                  let
                    go :: [Val] -> TEnv -> VEnv -> LEnv -> IO ICont -> IO Cont
                    go [] tenv'' venv'' (LEnv lenv'') icont'' = do
                      ic'' <- icont''
                      ic'' tenv venv
                    go (el:els) tenv'' venv'' (LEnv lenv'') icont'' =
                      return (\(state'', err'') -> do
                        ic''' <- icont''
                        let state''' = saveInStore state'' l el
                        --let (LEnv lenv''') = addContinueLabel (LEnv lenv'') (return (\valtype -> \val -> ic''' tenv'' venv''))
                        icont''' <- (transStmt stmt tenv'' venv'' (LEnv lenv) (return (\tenv''' -> \venv''' -> go els tenv''' venv''' (LEnv lenv'') icont'')))
                        icont''' (state''', err''))
                  fic <- go list tenv' venv' (addContinueLabel (LEnv lenv) (return (\valtype -> \val -> ic tenv' venv'))) icont
                  fic (state, err))
          ArrayVal a ->
            return (\(state, err) -> return (state, LoopError lineInfo))
          otherwise ->
            return (\(state, err) -> return (state, LoopError lineInfo)))
    ForRange lineInfo ident expr1 expr2 stmt -> do  --TODO
      ic <- icont
      ic tenv venv
    Composition lineInfo stmt1 stmt2 ->
      transStmt stmt1 tenv venv (LEnv lenv) (return (\tenv' -> \venv' -> (transStmt stmt2 tenv' venv' (LEnv lenv) icont)))
transType :: Type (Maybe (Int, Int)) -> ()
transType x =
  case x of
    Int lineInfo ->
      ()
    Str lineInfo ->
      ()
    Bool lineInfo ->
      ()
    Void lineInfo ->
      ()
    Float lineInfo ->
      ()
    List lineInfo fulltype ->
      ()
    Array lineInfo fulltype ->
      ()
    Fun lineInfo argtypes fulltype ->
      ()
transArgType :: ArgType (Maybe (Int, Int)) -> ()
transArgType x =
  case x of
    ArgType lineInfo argmod type_ ->
      ()
transFullType :: FullType (Maybe (Int, Int)) -> ()
transFullType x =
  case x of
    FullType lineInfo typemods type_ ->
      ()
transArgMod :: ArgMod a -> ()
transArgMod x =
  case x of
    AModVar lineInfo ->
      ()
    AModVal lineInfo ->
      ()
    AModInOut lineInfo ->
      ()
transTypeMod :: TypeMod a -> ()
transTypeMod x =
  case x of
    TModReadonly lineInfo ->
      ()
transExpr :: Expr (Maybe (Int, Int)) -> TEnv -> VEnv -> LEnv -> ECont -> IO (Cont)
transExpr x tenv venv (LEnv lenv) econt = do
  case x of
    EVar lineInfo ident ->
      case venv ident of
        Nothing ->
          return (\((next, store), err) -> return ((next, store), TypeError lineInfo))
        Just loc ->
          return (\((next, store), err) -> 
            case tenv ident of
              Nothing ->
                return ((next, store), TypeError lineInfo)
              Just fulltype -> do
                nc <- (econt fulltype (store loc))
                nc ((next, store), err))
    ELitInt lineInfo integer ->
      econt (readonlyIntT) (IntVal integer)
    ELitTrue lineInfo ->
      econt (readonlyBoolT) (BoolVal True)
    ELitFalse lineInfo ->
      econt (readonlyBoolT) (BoolVal False)
    EString lineInfo string ->
      econt (readonlyStringT) (StringVal string)
    ELitFloat lineInfo double ->
      econt (readonlyFloatT) (FloatVal double)
    EEmpList lineInfo fulltype ->
      econt (readonlyListT fulltype) (ListVal [])
    Neg lineInfo expr ->
      transExpr expr tenv venv (LEnv lenv) (\valtype -> \val -> econt valtype (negateNum val))
    Not lineInfo expr ->
      transExpr expr tenv venv (LEnv lenv) (\valtype -> \val -> econt valtype (negateBool val))
    ECons lineInfo expr1 expr2 ->
      transExpr expr1 tenv venv (LEnv lenv) (\eltype -> \el -> transExpr expr2 tenv venv (LEnv lenv) (\listtype -> \listval ->
        case listval of
          ListVal list -> 
            econt (readonlyListT listtype) (ListVal (el:list))
          otherwise ->
            return (\(state, err) -> return (state, TypeError lineInfo))
        ))
    EMul lineInfo expr1 mulop expr2 ->
      transExpr expr1 tenv venv (LEnv lenv) (\type1 -> \val1 ->
        transExpr expr2 tenv venv (LEnv lenv) (\type2 -> \val2 ->
          case transMulOp mulop val1 val2 of
            (restype, resval, NoError lineInfo) ->
              econt restype resval
            (_, _, err') ->
              return (\(state, err) -> return (state, err'))))
    EAdd lineInfo expr1 addop expr2 ->
      transExpr expr1 tenv venv (LEnv lenv) (\type1 -> \val1 ->
        transExpr expr2 tenv venv (LEnv lenv) (\type2 -> \val2 ->
          case transAddOp addop val1 val2 of
            (restype, resval, NoError lineInfo) ->
              econt restype resval
            (_, _, err') ->
              return (\(state, err) -> return (state, err'))))
    ERel lineInfo expr1 relop expr2 ->
      transExpr expr1 tenv venv (LEnv lenv) (\type1 -> \val1 ->
        transExpr expr2 tenv venv (LEnv lenv) (\type2 -> \val2 ->
          case transRelOp relop val1 val2 of
            (restype, resval, NoError lineInfo) ->
              econt restype resval
            (_, _, err') ->
              return (\(state, err) -> return (state, err'))))
    EAnd lineInfo expr1 andop expr2 ->
      transExpr expr1 tenv venv (LEnv lenv) (\type1 -> \val1 ->
        transExpr expr2 tenv venv (LEnv lenv) (\type2 -> \val2 ->
          case transAndOp andop val1 val2 of
            (restype, resval, NoError lineInfo) ->
              econt restype resval
            (_, _, err') ->
              return (\(state, err) -> return (state, err'))))
    EOr lineInfo expr1 orop expr2 ->
      transExpr expr1 tenv venv (LEnv lenv) (\type1 -> \val1 ->
        transExpr expr2 tenv venv (LEnv lenv) (\type2 -> \val2 ->
          case transOrOp orop val1 val2 of
            (restype, resval, NoError lineInfo) ->
              econt restype resval
            (_, _, err') ->
              return (\(state, err) -> return (state, err'))))
    EList lineInfo exprs ->
      let
        go :: [Expr (Maybe (Int, Int))] -> ECont -> Maybe (FullType (Maybe (Int, Int))) -> Val -> IO (Cont)
        go [] econt' (Just listtype) (ListVal list) =
          econt' (FullType lineInfo [TModReadonly lineInfo] (List lineInfo listtype)) (ListVal (reverse list))
        go (e:es) econt' Nothing NoVal =
          transExpr e tenv venv (LEnv lenv) (\valtype -> \val -> go es econt' (Just valtype) (ListVal [val]))
        go (e:es) econt' (Just listtype) (ListVal list) =
          transExpr e tenv venv (LEnv lenv) (\valtype -> \val ->
            if matchFullType listtype valtype then
              go es econt' (Just listtype) (ListVal (val:list)) 
            else
              return (\(state, err) -> return (state, TypeError lineInfo)))
        go _ _ _ _ =
          return (\(state, err) -> return (state, TypeError lineInfo))
      in
        go exprs tenv venv (LEnv lenv) econt Nothing NoVal
    EArr lineInfo exprs ->  --TODO
      econt (readonlyBoolT) (BoolVal True)
    EArrSize lineInfo fulltype expr ->  --TODO
      econt (readonlyBoolT) (BoolVal True)
    EApp lineInfo expr exprs ->
      transExpr expr tenv venv (LEnv lenv) (\functype -> \funcval ->
        case functype of
          Fun _ argtypes fulltype ->
            case funcval of
              FuncVal func ->
                let
                  go :: [Expr (Maybe (Int, Int))] -> [ArgVal] -> ([ArgVal] -> IO Cont) -> IO Cont
                  go [] argvals argvalcont =
                    argvalcont (reverse argvals)
                  go (e:es) argvals argvalcont =
                    transExpr e tenv venv (LEnv lenv) (\valtype -> \val ->
                      go es (val:argvals) argvalcont)
                in
                  go exprs [] (\argvals ->
                    func argtypes argvals ())
              otherwise ->
                return (\(state, err) -> return (state, TypeError lineInfo))
          otherwise -> 
            return (\(state, err) -> return (state, TypeError lineInfo)))
    EArrApp lineInfo expr exprs ->  --TODO
      econt (readonlyBoolT) (BoolVal True)
    EIf lineInfo expr1 expr2 expr3 ->
      transExpr expr1 tenv venv (LEnv lenv) (\type1 -> \val1 ->
        case val1 of
          BoolVal bval ->
            if bval then
              transExpr expr2 tenv venv (LEnv lenv) econt
            else
              transExpr expr3 tenv venv (LEnv lenv) econt
          otherwise ->
            return (\(state, err) -> return (state, TypeError lineInfo)))
    ELambda lineInfo fulltype args stmt ->  --TODO
      econt (readonlyBoolT) (BoolVal True)
    ERand lineInfo expr ->  --TODO
      econt (readonlyBoolT) (BoolVal True)
    ERandDist lineInfo expr1 expr2 ->  --TODO
      econt (readonlyBoolT) (BoolVal True)
    EProb lineInfo stmt expr ->  --TODO
      econt (readonlyBoolT) (BoolVal True)
    EProbSamp lineInfo expr1 stmt expr2 ->  --TODO
      econt (readonlyBoolT) (BoolVal True)
transAddOp :: AddOp (Maybe (Int, Int)) -> Val -> Val -> (FullType (Maybe (Int, Int)), Val, Error)
transAddOp x v1 v2 =
  case x of
    Plus lineInfo ->
      case (v1, v2) of
        (IntVal x1, IntVal x2) ->
          (readonlyIntT, IntVal (x1 + x2), NoError lineInfo)
        (FloatVal x1, FloatVal x2) ->
          (readonlyFloatT, FloatVal (x1 + x2), NoError lineInfo)
        otherwise ->
          (readonlyVoidT, NoVal, TypeError lineInfo)
    Minus lineInfo ->
      case (v1, v2) of
        (IntVal x1, IntVal x2) ->
          (readonlyIntT, IntVal (x1 - x2), NoError lineInfo)
        (FloatVal x1, FloatVal x2) ->
          (readonlyFloatT, FloatVal (x1 - x2), NoError lineInfo)
        otherwise ->
          (readonlyVoidT, NoVal, TypeError lineInfo)
transMulOp :: MulOp (Maybe (Int, Int)) -> Val -> Val -> (FullType (Maybe (Int, Int)), Val, Error)
transMulOp x v1 v2 =
  case x of
    Times lineInfo ->
      case (v1, v2) of
        (IntVal x1, IntVal x2) ->
          (readonlyIntT, IntVal (x1 * x2), NoError lineInfo)
        (FloatVal x1, FloatVal x2) ->
          (readonlyFloatT, FloatVal (x1 * x2), NoError lineInfo)
        otherwise ->
          (readonlyVoidT, NoVal, TypeError lineInfo)
    Div lineInfo ->
      case (v1, v2) of
        (IntVal x1, IntVal x2) ->
          if x2 == 0 then
            (readonlyVoidT, NoVal, DivisionBy0 lineInfo)
          else
            (readonlyIntT, IntVal (x1 `div` x2), NoError lineInfo)
        (FloatVal x1, FloatVal x2) ->
          if x2 == 0 then
            (readonlyVoidT, NoVal, DivisionBy0 lineInfo)
          else
            (readonlyFloatT, FloatVal (x1 / x2), NoError lineInfo)
        otherwise ->
          (readonlyVoidT, NoVal, TypeError lineInfo)
    Mod lineInfo ->
      case (v1, v2) of
        (IntVal x1, IntVal x2) ->
          if x2 == 0 then
            (readonlyVoidT, NoVal, DivisionBy0 lineInfo)
          else
            (readonlyIntT, IntVal (x1 `mod` x2), NoError lineInfo)
        otherwise ->
          (readonlyVoidT, NoVal, TypeError lineInfo)
transRelOp :: RelOp (Maybe (Int, Int)) -> Val -> Val -> (FullType (Maybe (Int, Int)), Val, Error)
transRelOp x v1 v2 =
  case x of
    LTH lineInfo ->
      case (v1, v2) of
        (IntVal x1, IntVal x2) ->
          (readonlyBoolT, BoolVal (x1 < x2), NoError lineInfo)
        otherwise ->
          (readonlyVoidT, NoVal, TypeError lineInfo)
    LE lineInfo ->
      case (v1, v2) of
        (IntVal x1, IntVal x2) ->
          (readonlyBoolT, BoolVal (x1 <= x2), NoError lineInfo)
        otherwise ->
          (readonlyVoidT, NoVal, TypeError lineInfo)
    GTH lineInfo ->
      case (v1, v2) of
        (IntVal x1, IntVal x2) ->
          (readonlyBoolT, BoolVal (x1 > x2), NoError lineInfo)
        otherwise ->
          (readonlyVoidT, NoVal, TypeError lineInfo)
    GE lineInfo ->
      case (v1, v2) of
        (IntVal x1, IntVal x2) ->
          (readonlyBoolT, BoolVal (x1 >= x2), NoError lineInfo)
        otherwise ->
          (readonlyVoidT, NoVal, TypeError lineInfo)
    EQU lineInfo ->
      case (v1, v2) of
        (IntVal x1, IntVal x2) ->
          (readonlyBoolT, BoolVal (x1 == x2), NoError lineInfo)
        otherwise ->
          (readonlyVoidT, NoVal, TypeError lineInfo)
    NE lineInfo ->
      case (v1, v2) of
        (IntVal x1, IntVal x2) ->
          (readonlyBoolT, BoolVal (x1 /= x2), NoError lineInfo)
        otherwise ->
          (readonlyVoidT, NoVal, TypeError lineInfo)
transOrOp :: OrOp (Maybe (Int, Int)) -> Val -> Val -> (FullType (Maybe (Int, Int)), Val, Error)
transOrOp x v1 v2 =
  case x of
    Or lineInfo ->
      case (v1, v2) of
        (BoolVal x1, BoolVal x2) ->
          (readonlyBoolT, BoolVal (x1 || x2), NoError lineInfo)
        otherwise ->
          (readonlyVoidT, NoVal, TypeError lineInfo)
transAndOp :: AndOp (Maybe (Int, Int)) -> Val -> Val -> (FullType (Maybe (Int, Int)), Val, Error)
transAndOp x v1 v2 = 
  case x of
    And lineInfo ->
      case (v1, v2) of
        (BoolVal x1, BoolVal x2) ->
          (readonlyBoolT, BoolVal (x1 && x2), NoError lineInfo)
        otherwise ->
          (readonlyVoidT, NoVal, TypeError lineInfo)

