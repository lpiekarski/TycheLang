module Tyche.TransStmt where

import           Tyche.Abs
import           Tyche.Converters
import           Tyche.Env
import           Tyche.State
import           Tyche.TransExpr
import           Tyche.Types

import           Data.Array

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
    let funcval = FuncVal (argsToArgTypes args) (\funcargs -> \callvenv -> transStmts stmts venvafterfuncvardef)
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
