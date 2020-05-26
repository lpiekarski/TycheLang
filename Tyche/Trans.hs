module Tyche.Trans where

import           Tyche.Abs
import           Tyche.Bool
import           Tyche.Env
import           Tyche.ErrM
import           Tyche.Helpers
import           Tyche.Internal  (internals)
import           Tyche.Numerical
import           Tyche.Print
import           Tyche.State
import           Tyche.Types

import           Data.Array


transIdent :: Ident -> ()
transIdent x = case x of
  Ident string -> ()
transProgram :: Program LineInfo -> Input -> Ans
transProgram x input = case x of
  Program lineinfo stmts -> do
    let defaultvenv = \ident -> Nothing
    let defaultlenv = \label -> Nothing
    let defaultstore = (0, \loc -> NoVal)
    let defaultans = (NoErr, "")
    let defaulticont =  (\venv -> (\state -> defaultans))
    let
      addInternals :: [(Ident, FullType LineInfo, Val)] -> VEnv -> Store -> (VEnv, Store)
      addInternals [] venv store = (venv, store)
      addInternals ((ident, _, val):ints) venv store = do
          let (varloc, storeaftervardef) = newLoc store
          let venvaftervardef = extendFunc venv ident (Just varloc)
          let storeaftervalsave = saveInStore storeaftervardef varloc val
          addInternals ints venvaftervardef storeaftervalsave
    let (venvwithinternals, storewithinternals) = addInternals internals defaultvenv defaultstore
    let defaultstate = (storewithinternals, input)
    let cont = transStmts stmts venvwithinternals defaultlenv defaulticont
    cont defaultstate
transArg :: Arg LineInfo -> ()
transArg x = case x of
  Arg _ argmod ident fulltype -> ()
transStmts :: [Stmt LineInfo] -> VEnv -> LEnv -> ICont -> Cont
transStmts x venv lenv icont = case x of
  [] -> icont venv
  stmt:stmts -> transStmt stmt venv lenv (\aftervenv -> transStmts stmts aftervenv lenv icont)
transStmt :: Stmt LineInfo -> VEnv -> LEnv -> ICont -> Cont
transStmt x venv lenv icont = case x of
  Skip _ -> icont venv
  Break _ -> case lenv LBreak of
    Nothing         -> errMsg "Break statement outside the loop\n"
    Just breakecont -> breakecont NoVal
  Continue _ -> case lenv LContinue of
    Nothing            ->  errMsg "Continue statement outside the loop\n"
    Just continueecont -> continueecont NoVal
  Ret _ expr -> case lenv LReturn of
    Nothing          ->  errMsg "Return statement outside the function\n"
    Just returnecont -> transExpr expr venv lenv returnecont
  VarDef _ ident fulltype expr ->
    transExpr expr venv lenv (\val -> \(store, input) -> do
      let (varloc, storeaftervardef) = newLoc store
      let venvaftervardef = extendFunc venv ident (Just varloc)
      let storeaftervalsave = saveInStore storeaftervardef varloc val
      let cont = icont venvaftervardef
      cont (storeaftervalsave, input))
  Ass _ ident expr ->
    transExpr expr venv lenv (\val -> (\(store, input) ->
      case venv ident of
        Nothing -> errMsg "Assigning value to undefined variable\n" (store, input)
        Just loc -> do
          let cont = icont venv
          cont (saveInStore store loc val, input)))
  FnDef _ ident fulltype args stmts -> \(store, input) -> do
    let (funcvarloc, storeafterfuncvardef) = newLoc store
    let venvafterfuncvardef = extendFunc venv ident (Just funcvarloc)
    let funcval = FuncVal (\funcargs -> \callvenv -> transStmts stmts venvafterfuncvardef)
    let storeafterfuncvarsave = saveInStore storeafterfuncvardef funcvarloc funcval
    let cont = icont venvafterfuncvardef
    cont (storeafterfuncvarsave, input)
  Cond _ expr stmts ->
    transExpr expr venv lenv (\val ->
      case val of
        BoolVal boolval ->
          if boolval then transStmts stmts venv lenv icont
          else icont venv
        otherwise -> errMsg "Value inside If expression is not bool\n")
  CondElse _ expr stmts1 stmts2 ->
    transExpr expr venv lenv (\val ->
      case val of
        BoolVal boolval ->
          if boolval then transStmts stmts1 venv lenv icont
          else transStmts stmts2 venv lenv icont
        otherwise -> errMsg "Value inside If expression is not bool\n")
  While _ expr stmts ->
    transExpr expr venv lenv (\val ->
      case val of
        BoolVal boolval ->
          if boolval then do
            let loopcont = transStmt x venv lenv icont
            let loopicont = \loopvenv -> loopcont
            let breaklenv = addBreakLabel lenv (\val -> icont venv)
            let lenvbreakcontinuelenv = addContinueLabel breaklenv (\val -> loopcont)
            transStmts stmts venv lenvbreakcontinuelenv loopicont
          else
            icont venv
        otherwise -> errMsg "Value inside While expression is not bool\n")
  ForList _ ident expr stmts -> do
    transExpr expr venv lenv (\val ->
      let
        list = case val of
          ListVal l  -> l
          ArrayVal a -> elems a
          otherwise  -> []
      in (\(store, input) -> do
        let (loopvarloc, storewithloopvardef) = newLoc store
        let venvwithloopvar = extendFunc venv ident (Just loopvarloc)
        let breaklenv = addBreakLabel lenv (\val -> icont venv)
        let
          iterate :: [Val] -> VEnv -> Cont -> Cont
          iterate [] itervenv itercont = itercont
          iterate (listel:listels) itervenv itercont = (\(iterstore, iterinput) -> do
            let storewithloopvarvalue = saveInStore iterstore loopvarloc listel
            let lenvbreakcontinuelenv = addContinueLabel breaklenv (\val -> iterate listels itervenv itercont)
            let aftericont = \aftervenv -> iterate listels aftervenv itercont
            let itercont = transStmts stmts venvwithloopvar lenvbreakcontinuelenv aftericont
            itercont (storewithloopvarvalue, iterinput))
        let startitericont = iterate list venv (icont venv)
        startitericont (store, input)))
  ForRange _ ident expr1 expr2 stmts ->
    transExpr expr1 venv lenv (\val1 ->
      transExpr expr2 venv lenv (\val2 ->
        case (val1, val2) of
          (IntVal intval1, IntVal intval2) -> (\(store, input) -> do
            let (loopvarloc, storewithloopvardef) = newLoc store
            let venvwithloopvar = extendFunc venv ident (Just loopvarloc)
            let breaklenv = addBreakLabel lenv (\val -> icont venv)
            let
              iterate :: Integer -> Integer -> Integer -> VEnv -> ICont -> Cont
              iterate iterval targetval direction itervenv itericont =
                if iterval - direction == targetval then do
                  let itericont = itericont
                  itericont itervenv
                else \(iterstore, iterinput) -> do
                  let storewithloopvarvalue = saveInStore iterstore loopvarloc (IntVal iterval)
                  let lenvbreakcontinuelenv = addContinueLabel breaklenv (\val -> iterate (iterval + direction) targetval direction itervenv itericont)
                  let aftericont = \aftervenv -> iterate (iterval + direction) targetval direction aftervenv itericont
                  let itercont = transStmts stmts venvwithloopvar lenvbreakcontinuelenv aftericont
                  itercont (storewithloopvarvalue, iterinput)
            let startitericont = iterate intval1 intval2 (if intval1 < intval2 then 1 else -1) venv icont
            startitericont (store, input))
          otherwise -> errMsg "Expected int value\n"
      ))
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
transExpr :: Expr LineInfo -> VEnv -> LEnv -> ECont -> Cont
transExpr x venv lenv econt = case x of
  ELitVoid _ -> econt NoVal
  EVar _ ident -> case venv ident of
    Nothing -> errMsg ("Undefined variable " ++ (printTree ident))
    Just loc -> \(store@(next, storef), input) -> do
      let cont = econt (storef loc)
      cont (store, input)
  ELitInt _ integer -> econt (IntVal integer)
  ELitTrue _ -> econt (BoolVal True)
  ELitFalse _ -> econt (BoolVal False)
  EString _ string -> econt (StringVal string)
  ELitFloat _ double -> econt (FloatVal double)
  EEmpList _ fulltype -> econt (ListVal [])
  EApp _ expr exprs ->
    transExpr expr venv lenv (\val ->
      case val of
        FuncVal argtypes func -> \(store, input) -> (func (exprsToArgs exprs) venv lenv (\v -> econt NoVal)) (store, input)
        otherwise    -> errMsg "Expected a function\n")
  Neg _ expr -> transExpr expr venv lenv (\val -> econt (negateNumerical val))
  Not _ expr -> transExpr expr venv lenv (\val -> econt (negateBool val))
  ECons _ expr1 expr2 ->
    transExpr expr1 venv lenv (\val1 ->
      transExpr expr2 venv lenv (\val2 ->
        case val2 of
          ListVal list -> econt (ListVal (val1:list))
          otherwise    -> errMsg "Expected list value\n"))
  EMul _ expr1 mulop expr2 ->
    transExpr expr1 venv lenv (\val1 ->
      transExpr expr2 venv lenv (\val2 ->
        case transMulOp mulop val1 val2 of
          (resval, NoErr) -> econt resval
          (_, ErrMsg str) -> errMsg str))
  EAdd _ expr1 addop expr2 ->
    transExpr expr1 venv lenv (\val1 ->
      transExpr expr2 venv lenv (\val2 ->
        case transAddOp addop val1 val2 of
          (resval, NoErr) -> econt resval
          (_, ErrMsg str) -> errMsg str))
  ERel _ expr1 relop expr2 ->
    transExpr expr1 venv lenv (\val1 ->
      transExpr expr2 venv lenv (\val2 ->
        case transRelOp relop val1 val2 of
          (resval, NoErr) -> econt resval
          (_, ErrMsg str) -> errMsg str))
  EAnd _ expr1 andop expr2 ->
    transExpr expr1 venv lenv (\val1 ->
      case val1 of
        BoolVal boolval1 ->
          if boolval1 then
            transExpr expr2 venv lenv (\val2 ->
              case transAndOp andop val1 val2 of
                (resval, NoErr) -> econt resval
                (_, ErrMsg str) -> errMsg str)
          else
            econt (BoolVal False)
        otherwise -> errMsg "Expected bool value\n")
  EOr _ expr1 orop expr2 ->
    transExpr expr1 venv lenv (\val1 ->
      transExpr expr2 venv lenv (\val2 ->
        case transOrOp orop val1 val2 of
          (resval, NoErr) -> econt resval
          (_, ErrMsg str) -> errMsg str))
  EList _ exprs ->
    let
      buildList :: [Expr LineInfo] ->  ECont -> Val ->  Cont
      buildList [] buildecont (ListVal acc) =
        buildecont (ListVal (reverse acc))
      buildList (e:es) buildecont (ListVal acc) =
        transExpr e venv lenv (\val -> buildList es buildecont (ListVal (val:acc)))
    in
      buildList exprs econt (ListVal [])
  EArr _ exprs -> econt NoVal
  EArrSize _ fulltype expr -> econt NoVal
  EArrApp _ expr1 expr2 -> econt NoVal
  EIf _ expr1 expr2 expr3       -> econt NoVal
  ELambda _ fulltype args stmts -> econt NoVal
  ERand _ expr                  -> econt NoVal
  ERandDist _ expr1 expr2 -> econt NoVal
  EProbSamp _ expr1 stmts expr2 -> econt NoVal
transAddOp :: AddOp LineInfo -> Val -> Val -> (Val, ErrorType)
transAddOp x v1 v2 = case x of
  Plus _  -> (addNumericals v1 v2, NoErr)
  Minus _ -> (substractNumericals v1 v2, NoErr)
transMulOp :: MulOp LineInfo -> Val -> Val -> (Val, ErrorType)
transMulOp x v1 v2 = case x of
  Times _ -> (multiplyNumericals v1 v2, NoErr)
  Div _   -> case v2 of
    FloatVal 0 -> (NoVal, ErrMsg "Encountered division by 0\n")
    IntVal 0   -> (NoVal, ErrMsg "Encountered division by 0\n")
    otherwise  -> (divideNumericals v1 v2, NoErr)
  Mod _   -> case v2 of
    FloatVal 0 -> (NoVal, ErrMsg "Encountered division by 0\n")
    IntVal 0   -> (NoVal, ErrMsg "Encountered division by 0\n")
    otherwise  -> (modNumericals v1 v2, NoErr)
transRelOp :: RelOp LineInfo -> Val -> Val -> (Val, ErrorType)
transRelOp x v1 v2 = case x of
  LTH _ -> (lthNumericals v1 v2, NoErr)
  LE _  -> (leNumericals v1 v2, NoErr)
  GTH _ -> (gthNumericals v1 v2, NoErr)
  GE _  -> (geNumericals v1 v2, NoErr)
  EQU _ -> (equNumericals v1 v2, NoErr)
  NE _  -> (neNumericals v1 v2, NoErr)
transOrOp :: OrOp LineInfo -> Val -> Val -> (Val, ErrorType)
transOrOp x v1 v2 = case x of
  Or _ -> case (v1, v2) of
    (BoolVal x1, BoolVal x2) -> (BoolVal (x1 || x2), NoErr)
transAndOp :: AndOp LineInfo -> Val -> Val -> (Val, ErrorType)
transAndOp x v1 v2 = case x of
  And _ -> case (v1, v2) of
    (BoolVal x1, BoolVal x2) -> (BoolVal (x1 && x2), NoErr)
