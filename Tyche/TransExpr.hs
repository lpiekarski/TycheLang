module Tyche.TransExpr where

import           Tyche.Abs
import           Tyche.Bool
import           Tyche.Env
import           Tyche.Numerical
import           Tyche.Print
import           Tyche.Trans
import           Tyche.Types

import           Data.Array

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
  EEmpArray _ fulltype -> econt (ArrayVal (listArray (0, 0) []))
  EApp _ expr exprs -> --TODO args
    transExpr expr venv lenv (\val ->
      case val of
        FuncVal argtypes func ->
          let
            exprsToArgVals :: [Expr LineInfo] -> [ArgType LineInfo] -> [ArgVal] -> ([ArgVal] -> Cont) -> Cont
            exprsToArgVals [] [] acc avcont = avcont (reverse acc)
            exprsToArgVals (e:es) ((ArgType _ argmod fulltype):ats) acc avcont =
              transExpr e venv lenv (\val ->
                {-let
                  av = case argmod of
                    AModVar _ -> Variable loc ident
                    AModVal _ -> Value val ident
                    AModInOut _ ->
                in-}
                  exprsToArgVals es ats ({-av:-}acc) avcont
              )
          in
          \(store, input) -> (func []{-(exprsToArgVals exprs argtypes [] )-} venv (addReturnLabel lenv econt) (\v -> econt NoVal)) (store, input)
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
      buildList :: [Expr LineInfo] ->  ECont -> Val -> Cont
      buildList [] buildecont (ListVal acc) =
        buildecont (ListVal (reverse acc))
      buildList (e:es) buildecont (ListVal acc) =
        transExpr e venv lenv (\val -> buildList es buildecont (ListVal (val:acc)))
    in
      buildList exprs econt (ListVal [])
  EArr _ exprs ->
    let
      buildList :: [Expr LineInfo] -> ECont -> [Val] -> Cont
      buildList [] buildecont acc =
        buildecont (ArrayVal (listArray (0, (length acc) - 1) (reverse acc)))
      buildList (e:es) buildecont acc =
        transExpr e venv lenv (\val -> buildList es buildecont (val:acc))
    in
      buildList exprs econt []
  EArrSize _ fulltype expr -> econt NoVal --TODO
  EArrApp _ expr1 expr2 -> econt NoVal --TODO
  EIf _ expr1 expr2 expr3       -> econt NoVal --TODO
  ELambda _ fulltype args stmts -> econt NoVal --TODO
  ERand _ expr                  -> econt NoVal --TODO
  ERandDist _ expr1 expr2 -> econt NoVal --TODO
  EProbSamp _ expr1 stmts expr2 -> econt NoVal --TODO