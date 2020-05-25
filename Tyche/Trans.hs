module Tyche.Trans where

import           Tyche.Abs
import           Tyche.Bool
import           Tyche.Env
import           Tyche.ErrM
import           Tyche.Helpers
import           Tyche.Numerical
import           Tyche.Print
import           Tyche.State
import           Tyche.Types

import           Data.Array


transIdent :: Ident -> ()
transIdent x = case x of
  Ident string -> ()
transProgram :: Program LineInfo -> IO (Err ())
transProgram x = case x of
  Program lineinfo stmts -> do
    let defaultvenv = \v -> Nothing
    let defaultlenv = LEnv (\v -> Nothing)
    let defaultioicont = return (\venv -> return (\state -> return state))
    let defaultstore = (0, \l -> NoVal)
    let defaultstacktrace = [(Nothing, FullType Nothing [] (Fun Nothing [] voidT))]
    let defaulterr = (NoErr, defaultstacktrace)
    let defaultstate = (defaultstore, defaulterr)
    cont <- transStmts stmts defaultvenv defaultlenv defaultioicont
    (finalstore, finalerr) <- cont defaultstate
    putStr $ printStore finalstore
    case finalerr of
      (NoErr, _) -> return (Ok ())
      otherwise  -> return (Bad (printError finalerr))
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
  Break _ -> case lenv LBreak of
    Nothing -> return (\(store, (errtype, stacktrace)) -> return (store, (ErrMsg "Break statement outside the loop", stacktrace)))
    Just breakioecont -> do
      breakecont <- breakioecont
      breakecont NoVal
  Continue _ -> case lenv LContinue of
    Nothing -> return (\(store, (errtype, stacktrace)) -> return (store, (ErrMsg "Continue statement outside the loop", stacktrace)))
    Just continueioecont -> do
      continueecont <- continueioecont
      continueecont NoVal
  Ret _ expr -> case lenv LReturn of
    Nothing -> return (\(store, (errtype, stacktrace)) -> return (store, (ErrMsg "Return statement outside the function", stacktrace)))
    Just returnioecont -> transExpr expr venv (LEnv lenv) returnioecont
  VarDef _ ident fulltype expr ->
    transExpr expr venv (LEnv lenv) (return (\val -> return (\(store, err) -> do
      let (varloc, storeaftervardef) = newLoc store
      let venvaftervardef = extendFunc venv ident (Just varloc)
      let storeaftervalsave = saveInStore storeaftervardef varloc val
      icont <- ioicont
      cont <- icont venvaftervardef
      cont (storeaftervalsave, err))))
  Ass _ ident expr ->
    transExpr expr venv (LEnv lenv) (return (\val -> return (\(store, err@(errtype, stacktrace)) ->
      case venv ident of
        Nothing -> return (store, (ErrMsg "Assigning value to undefined variable", stacktrace))
        Just loc -> do
          icont <- ioicont
          cont <- icont venv
          cont (saveInStore store loc val, err))))
  FnDef _ ident fulltype args stmts -> return (\(store, err) -> do
    let (funcvarloc, storeafterfuncvardef) = newLoc store
    let venvafterfuncvardef = extendFunc venv ident (Just funcvarloc)
    let funcval = FuncVal (transStmts stmts venvafterfuncvardef)
    let storeafterfuncvarsave = saveInStore storeafterfuncvardef funcvarloc funcval
    icont <- ioicont
    cont <- icont venvafterfuncvardef
    cont (storeafterfuncvarsave, err))
  Cond _ expr stmts ->
    transExpr expr venv (LEnv lenv) (return (\val ->
      case val of
        BoolVal boolval ->
          if boolval then
            transStmts stmts venv (LEnv lenv) ioicont
          else do
            icont <- ioicont
            icont venv
        otherwise -> return (\(state, (errtype, stacktrace)) -> return (state, (ErrMsg "Value inside If expression is not bool", stacktrace)))))
  CondElse _ expr stmts1 stmts2 ->
    transExpr expr venv (LEnv lenv) (return (\val ->
      case val of
        BoolVal boolval ->
          if boolval then
            transStmts stmts1 venv (LEnv lenv) ioicont
          else
            transStmts stmts2 venv (LEnv lenv) ioicont
        otherwise -> return (\(state, (errtype, stacktrace)) -> return (state, (ErrMsg "Value inside If expression is not bool", stacktrace)))))
  While _ expr stmts ->
    transExpr expr venv (LEnv lenv) (return (\val ->
      case val of
        BoolVal boolval ->
          if boolval then do
            let loopiocont = transStmt x venv (LEnv lenv) ioicont
            let loopioicont = return (\loopvenv -> loopiocont)
            icont <- ioicont
            let (LEnv breaklenv) = addBreakLabel (LEnv lenv) (return (\val -> icont venv))
            let (LEnv breakcontinuelenv) = addContinueLabel (LEnv breaklenv) (return (\val -> loopiocont))
            transStmts stmts venv (LEnv breakcontinuelenv) loopioicont
          else do
            icont <- ioicont
            icont venv
        otherwise -> return (\(state, (errtype, stacktrace)) -> return (state, (ErrMsg "Value inside While expression is not bool", stacktrace)))))
  ForList _ ident expr stmts -> do -- TODO fix loop variable leaking out of the loop
    transExpr expr venv (LEnv lenv) (return (\val ->
      let
        list = case val of
          ListVal l  -> l
          ArrayVal a -> elems a
          otherwise  -> []
      in return (\(store, err) -> do
        let (loopvarloc, storewithloopvardef) = newLoc store
        let venvwithloopvar = extendFunc venv ident (Just loopvarloc)
        icont <- ioicont
        let (LEnv breaklenv) = addBreakLabel (LEnv lenv) (return (\val -> icont venv))
        let
          iterate :: [Val] -> VEnv -> IO ICont -> IO Cont
          iterate [] itervenv iterioicont = do
            itericont <- iterioicont
            itericont itervenv
          iterate (listel:listels) itervenv iterioicont = return (\(iterstore, itererr) -> do
            let storewithloopvarvalue = saveInStore iterstore loopvarloc listel
            let (LEnv breakcontinuelenv) = addContinueLabel (LEnv breaklenv) (return (\val -> iterate listels itervenv iterioicont))
            let afterioicont = return (\aftervenv -> iterate listels aftervenv iterioicont)
            itercont <- transStmts stmts venvwithloopvar (LEnv breakcontinuelenv) afterioicont
            itercont (storewithloopvarvalue, itererr))
        startitericont <- iterate list venv ioicont
        startitericont (store, err))))
  ForRange _ ident expr1 expr2 stmts ->
    transExpr expr1 venv (LEnv lenv) (return (\val1 ->
      transExpr expr2 venv (LEnv lenv) (return (\val2 ->
        case (val1, val2) of
          (IntVal intval1, IntVal intval2) -> return (\(store, err) -> do
            let (loopvarloc, storewithloopvardef) = newLoc store
            let venvwithloopvar = extendFunc venv ident (Just loopvarloc)
            icont <- ioicont
            let (LEnv breaklenv) = addBreakLabel (LEnv lenv) (return (\val -> icont venv))
            let
              iterate :: Integer -> Integer -> Integer -> VEnv -> IO ICont -> IO Cont
              iterate iterval targetval direction itervenv iterioicont =
                if iterval - direction == targetval then do
                  itericont <- iterioicont
                  itericont itervenv
                else return (\(iterstore, itererr) -> do
                  let storewithloopvarvalue = saveInStore iterstore loopvarloc (IntVal iterval)
                  let (LEnv breakcontinuelenv) = addContinueLabel (LEnv breaklenv) (return (\val -> iterate (iterval + direction) targetval direction itervenv iterioicont))
                  let afterioicont = return (\aftervenv -> iterate (iterval + direction) targetval direction aftervenv iterioicont)
                  itercont <- transStmts stmts venvwithloopvar (LEnv breakcontinuelenv) afterioicont
                  itercont (storewithloopvarvalue, itererr))
            startitericont <- iterate intval1 intval2 (if intval1 < intval2 then 1 else -1) venv ioicont
            startitericont (store, err))
          otherwise -> return (\(state, (errtype, stacktrace)) -> return (state, (ErrMsg "Expected int value", stacktrace)))
      ))))
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
transExpr :: Expr LineInfo -> VEnv -> LEnv -> IO ECont -> IO Cont
transExpr x venv (LEnv lenv) ioecont = case x of
  ELitVoid _ -> do
    econt <- ioecont
    econt NoVal
  EVar _ ident -> case venv ident of
    Nothing -> return (\(store, (errtype, stacktrace)) -> return (store, (ErrMsg ("Undefined variable " ++ (printTree ident)), stacktrace)))
    Just loc -> return (\(store@(next, storef), err) -> do
      econt <- ioecont
      cont <- econt (storef loc)
      cont (store, err))
  ELitInt _ integer -> do
    econt <- ioecont
    econt (IntVal integer)
  ELitTrue _ -> do
    econt <- ioecont
    econt (BoolVal True)
  ELitFalse _ -> do
    econt <- ioecont
    econt (BoolVal False)
  EString _ string -> do
    econt <- ioecont
    econt (StringVal string)
  ELitFloat _ double -> do
    econt <- ioecont
    econt (FloatVal double)
  EEmpList _ fulltype -> do
    econt <- ioecont
    econt (ListVal [])
  EApp _ expr exprs -> do -- TODO
    econt <- ioecont
    econt NoVal
  Neg _ expr ->
    transExpr expr venv (LEnv lenv) (return (\val -> do
      econt <- ioecont
      econt (negateNumerical val)))
  Not _ expr ->
    transExpr expr venv (LEnv lenv) (return (\val -> do
      econt <- ioecont
      econt (negateBool val)))
  ECons _ expr1 expr2 ->
    transExpr expr1 venv (LEnv lenv) (return (\val1 ->
      transExpr expr2 venv (LEnv lenv) (return (\val2 ->
        case val2 of
          ListVal list -> do
            econt <- ioecont
            econt (ListVal (val1:list))
          otherwise -> return (\(store, (errtype, stacktrace)) -> return (store, (ErrMsg "Expected list value", stacktrace)))
      ))))
  EMul _ expr1 mulop expr2 ->
    transExpr expr1 venv (LEnv lenv) (return (\val1 ->
      transExpr expr2 venv (LEnv lenv) (return (\val2 ->
        case transMulOp mulop val1 val2 of
          (resval, (NoErr, _)) -> do
            econt <- ioecont
            econt resval
          (_, (mulerrtype, _)) -> return (\(state, (errtype, stacktrace)) -> return (state,(mulerrtype, stacktrace)))
      ))))
  EAdd _ expr1 addop expr2 ->
    transExpr expr1 venv (LEnv lenv) (return (\val1 ->
      transExpr expr2 venv (LEnv lenv) (return (\val2 ->
        case transAddOp addop val1 val2 of
          (resval, (NoErr, _)) -> do
            econt <- ioecont
            econt resval
          (_, (adderrtype, _)) -> return (\(state, (errtype, stacktrace)) -> return (state,(adderrtype, stacktrace)))
      ))))
  ERel _ expr1 relop expr2 ->
    transExpr expr1 venv (LEnv lenv) (return (\val1 ->
      transExpr expr2 venv (LEnv lenv) (return (\val2 ->
        case transRelOp relop val1 val2 of
          (resval, (NoErr, _)) -> do
            econt <- ioecont
            econt resval
          (_, (relerrtype, _)) -> return (\(state, (errtype, stacktrace)) -> return (state,(relerrtype, stacktrace)))
      ))))
  EAnd _ expr1 andop expr2 ->
    transExpr expr1 venv (LEnv lenv) (return (\val1 ->
      case val1 of
        BoolVal boolval1 ->
          if boolval1 then
            transExpr expr2 venv (LEnv lenv) (return (\val2 ->
              case transAndOp andop val1 val2 of
                (resval, (NoErr, _)) -> do
                  econt <-ioecont
                  econt resval
                (_, (anderrtype, _)) -> return (\(state, (errtype, stacktrace)) -> return (state,(anderrtype, stacktrace)))
            ))
          else do
            econt <- ioecont
            econt (BoolVal False)
        otherwise -> return (\(state, (errtype, stacktrace)) -> return (state,(ErrMsg "Expected bool value", stacktrace)))
      ))
  EOr _ expr1 orop expr2 ->
    transExpr expr1 venv (LEnv lenv) (return (\val1 ->
      transExpr expr2 venv (LEnv lenv) (return (\val2 ->
        case transOrOp orop val1 val2 of
          (resval, (NoErr, _)) -> do
            econt <- ioecont
            econt resval
          (_, (orerrtype, _)) -> return (\(state, (errtype, stacktrace)) -> return (state,(orerrtype, stacktrace)))
      ))))
  EList _ exprs ->
    let
      buildList :: [Expr LineInfo] -> IO ECont -> Val -> IO Cont
      buildList [] buildioecont (ListVal acc) = do
        buildecont <- buildioecont
        buildecont (ListVal (reverse acc))
      buildList (e:es) buildioecont (ListVal acc) =
        transExpr e venv (LEnv lenv) (return (\val -> buildList es buildioecont (ListVal (val:acc))))
    in
      buildList exprs ioecont (ListVal [])
  EArr _ exprs                  -> do
    econt <- ioecont
    econt NoVal
  EArrSize _ fulltype expr      -> do
    econt <- ioecont
    econt NoVal
  EArrApp _ expr1 expr2 -> do
    econt <- ioecont
    econt NoVal
  EIf _ expr1 expr2 expr3       -> do
    econt <- ioecont
    econt NoVal
  ELambda _ fulltype args stmts -> do
    econt <- ioecont
    econt NoVal
  ERand _ expr                  -> do
    econt <- ioecont
    econt NoVal
  ERandDist _ expr1 expr2 -> do
    econt <- ioecont
    econt NoVal
  EProbSamp _ expr1 stmts expr2 -> do
    econt <- ioecont
    econt NoVal
transAddOp :: AddOp LineInfo -> Val -> Val -> (Val, Error)
transAddOp x v1 v2 = case x of
  Plus _  -> (addNumericals v1 v2, (NoErr, []))
  Minus _ -> (substractNumericals v1 v2, (NoErr, []))
transMulOp :: MulOp LineInfo -> Val -> Val -> (Val, Error)
transMulOp x v1 v2 = case x of
  Times _ -> (multiplyNumericals v1 v2, (NoErr, []))
  Div _   -> case v2 of
    FloatVal 0 -> (NoVal, (ErrMsg "Encountered division by 0", []))
    IntVal 0   -> (NoVal, (ErrMsg "Encountered division by 0", []))
    otherwise  -> (divideNumericals v1 v2, (NoErr, []))
  Mod _   -> case v2 of
    FloatVal 0 -> (NoVal, (ErrMsg "Encountered division by 0", []))
    IntVal 0   -> (NoVal, (ErrMsg "Encountered division by 0", []))
    otherwise  -> (modNumericals v1 v2, (NoErr, []))
transRelOp :: RelOp LineInfo -> Val -> Val -> (Val, Error)
transRelOp x v1 v2 = case x of
  LTH _ -> (lthNumericals v1 v2, (NoErr, []))
  LE _  -> (leNumericals v1 v2, (NoErr, []))
  GTH _ -> (gthNumericals v1 v2, (NoErr, []))
  GE _  -> (geNumericals v1 v2, (NoErr, []))
  EQU _ -> (equNumericals v1 v2, (NoErr, []))
  NE _  -> (neNumericals v1 v2, (NoErr, []))
transOrOp :: OrOp LineInfo -> Val -> Val -> (Val, Error)
transOrOp x v1 v2 = case x of
  Or _ -> case (v1, v2) of
    (BoolVal x1, BoolVal x2) -> (BoolVal (x1 || x2), (NoErr, []))
transAndOp :: AndOp LineInfo -> Val -> Val -> (Val, Error)
transAndOp x v1 v2 = case x of
  And _ -> case (v1, v2) of
    (BoolVal x1, BoolVal x2) -> (BoolVal (x1 && x2), (NoErr, []))
