module Tyche.TypeCheck where

import           Tyche.Abs
import           Tyche.ErrM
import           Tyche.Types

typecheckNothing :: Maybe a -> TEnv -> FullType LineInfo -> Bool -> Bool -> TypeCheckResult
typecheckNothing Nothing tenv functype returned inloop = Ok (voidT, tenv, functype, returned, inloop)
typecheckIdent :: Ident -> TEnv -> FullType LineInfo -> Bool -> Bool -> TypeCheckResult
typecheckIdent x tenv functype returned inloop = case x of
  Ident string -> Ok (voidT, tenv, functype, returned, inloop)
typecheckStmts :: [Stmt LineInfo] -> TEnv -> FullType LineInfo -> Bool -> Bool -> TypeCheckResult
typecheckStmts x tenv functype returned inloop = case x of
  [] -> Ok (voidT, tenv, functype, returned, inloop)
  stmt:stmts ->
    passWithErrorHandle (typecheckStmt stmt tenv functype returned inloop) typecheckStmts Nothing stmts
typecheckProgram :: Program LineInfo -> TypeCheckResult
typecheckProgram x tenv functype returned inloop = case x of
  Program _ stmts ->
    typecheckStmts stmts (\v -> Nothing) voidT False False
typecheckArg :: [Arg LineInfo] -> TEnv -> FullType LineInfo -> Bool -> Bool -> TypeCheckResult
typecheckArg x tenv functype returned inloop = case x of
  Arg _ argmod ident fulltype -> Ok (voidT, tenv, functype, returned, inloop)
typecheckStmt :: [Stmt LineInfo] -> TEnv -> FullType LineInfo -> Bool -> Bool -> TypeCheckResult
typecheckStmt x tenv functype returned inloop = case x of
  Skip _                                -> Ok (voidT, tenv, functype, returned, inloop)
  Break lineInfo                               ->
    if inloop then
      Ok (voidT, tenv, functype, returned, inloop)
    else
      Bad (" at Break Statement " ++ (lineInfoString lineInfo) ++ " Break statement outside of the loop\n")
  Continue lineInfo                            ->
    if inloop then
      Ok (voidT, tenv, functype, returned, inloop)
    else
      Bad (" at Continue Statement " ++ (lineInfoString lineInfo) ++ " Continue statement outside of the loop\n")
  Ret lineInfo expr ->
    case typecheckExpr expr tenv functype returned inloop of
      Bad str -> Bad (str ++ " at Return Statement " ++ (lineInfoString lineInfo) ++ "\n")
      Ok (exprtype, _, _, _, _) ->
        if matchFullType exprtype functype then
          Ok (voidT, tenv, functype, True, inloop)
        else
          Bad (" at Return Statement " ++ (lineInfoString lineInfo) ++ " Return value type `" ++ (show exprtype) ++ "` doesn't match expected function return type `" ++ (show functype) ++ "`\n")
  VarDef lineInfo ident fulltype expr ->
    case typecheckExpr expr tenv functype returned inloop of
      Bad str -> Bad (str ++ " at Variable Definition " ++ (lineInfoString lineInfo) ++ "\n")
      Ok (exprtype, _, _, _, _) ->
        if matchFullType exprtype fulltype then
          Ok (voidT, extendTEnv tenv ident fulltype, functype, returned, inloop)
        else
          Bad (" at Variable Definition " ++ (lineInfoString lineInfo) ++ " Unable to match types in variable definition. Expected `" ++ (show fulltype) ++ "`, got `" ++ (show exprtype) ++ "`\n")
  Ass lineInfo ident expr ->
    case tenv ident of
      Nothing -> Bad (" at Assignment Statement " ++ (lineInfoString lineInfo) ++ " Assigning value to an undeclared variable `" ++ (show ident) ++ "`\n")
      Just fulltype ->
        case typeCheckExpr expr tenv functype returned inloop of
          Bad str -> Bad (str ++ " at Assignment Statement " ++ (lineInfoString lineInfo) ++ "\n")
          Ok (exprtype, _, _, _, _) ->
            if matchFullType exprtype fulltype then
              Ok (voidT, tenv, functype, returned, inloop)
            else
              Bad (" at Assignment Statement " ++ (lineInfoString lineInfo) ++ " Unable to match types in assignment. Expected `" ++ (show fulltype) ++ "`, got `" ++ (show exprtype) ++ "`\n")
  FnDef lineInfo ident fulltype args stmts ->
    case typecheckStmts stmts (extendTEnv (extendTEnvArgs tenv args) ident fulltype) fulltype False False of
      Bad str -> Bad (str ++ " at Function Definition " ++ (lineInfoString lineInfo) ++ "\n")
      Ok (_, _, _, returned', _) ->
        if returned' then
          Ok (voidT, tenv, functype, returned, inloop)
        else
          Bad (" at Function Definition " ++ (lineInfoString lineInfo) ++ " Function returns no value (return statement is missing)\n")
  Cond lineInfo expr stmts ->
    case typecheckExpr expr tenv functype returned inloop of
      Bad str -> Bad (str ++ " at If Statement " ++ (lineInfoString lineInfo) ++ "\n")
      Ok (exprtype, _, _, _, _) ->
        if matchFullType exprtype boolT then
          case typecheckStmts stmts tenv functype returned inloop of
            Bad str -> Bad (str ++ " at If Statement " ++ (lineInfoString lineInfo) ++ "\n")
            Ok (_, _, _, _, _) -> Ok (voidT, tenv, functype, returned, inloop)
        else
          Bad (" at If Statement " ++ (lineInfoString lineInfo) ++ " Expected expression type `" ++ (show boolT) ++ "`, got `" ++ (show exprtype) ++ "`\n")
  CondElse _ expr stmts1 stmts2         -> failure x
  While _ expr stmts                    -> failure x
  ForList _ ident expr stmts            -> failure x
  ForRange _ ident expr1 expr2 stmts    -> failure x
typecheckType :: [Type LineInfo] -> TEnv -> FullType LineInfo -> Bool -> Bool -> TypeCheckResult
typecheckType x tenv functype returned inloop = case x of
  Int _                   -> Ok (voidT, tenv, functype, returned, inloop)
  Str _                   -> Ok (voidT, tenv, functype, returned, inloop)
  Bool _                  -> Ok (voidT, tenv, functype, returned, inloop)
  Void _                  -> Ok (voidT, tenv, functype, returned, inloop)
  Float _                 -> Ok (voidT, tenv, functype, returned, inloop)
  List lineInfo fulltype         -> passWithErrorHandle (typecheckFullType fulltype tenv functype returned inloop) typecheckNothing lineInfo Nothing
  Array lineInfo fulltype        -> passWithErrorHandle (typecheckFullType fulltype tenv functype returned inloop) typecheckNothing lineInfo Nothing
  Fun _ argtypes fulltype -> Ok (tenv, functype, returned, inloop)
typecheckArgType :: [ArgType LineInfo] -> TEnv -> FullType LineInfo -> Bool -> Bool -> TypeCheckResult
typecheckArgType x tenv functype returned inloop = case x of
  ArgType _ argmod fulltype -> failure x
typecheckFullType :: [FullType LineInfo] -> TEnv -> FullType LineInfo -> Bool -> Bool -> TypeCheckResult
typecheckFullType x tenv functype returned inloop = case x of
  FullType _ typemods type_ -> failure x
typecheckArgMod :: [ArgMod LineInfo] -> TEnv -> FullType LineInfo -> Bool -> Bool -> TypeCheckResult
typecheckArgMod x tenv functype returned inloop = case x of
  AModVar _   -> failure x
  AModVal _   -> failure x
  AModInOut _ -> failure x
typecheckTypeMod :: [TypeMod LineInfo] -> TEnv -> FullType LineInfo -> Bool -> Bool -> TypeCheckResult
typecheckTypeMod x tenv functype returned inloop = case x of
  TModReadonly _ -> failure x
typecheckExpr :: [Expr LineInfo] -> TEnv -> FullType LineInfo -> Bool -> Bool -> TypeCheckResult
typecheckExpr x tenv functype returned inloop = case x of
  ELitVoid _                    -> failure x
  EVar _ ident                  -> failure x
  ELitInt _ integer             -> failure x
  ELitTrue _                    -> failure x
  ELitFalse _                   -> failure x
  EString _ string              -> failure x
  ELitFloat _ double            -> failure x
  EEmpList _ fulltype           -> failure x
  EApp _ expr exprs             -> failure x
  Neg _ expr                    -> failure x
  Not _ expr                    -> failure x
  ECons _ expr1 expr2           -> failure x
  EMul _ expr1 mulop expr2      -> failure x
  EAdd _ expr1 addop expr2      -> failure x
  ERel _ expr1 relop expr2      -> failure x
  EAnd _ expr1 andop expr2      -> failure x
  EOr _ expr1 orop expr2        -> failure x
  EList _ exprs                 -> failure x
  EArr _ exprs                  -> failure x
  EArrSize _ fulltype expr      -> failure x
  EArrApp _ expr1 expr2         -> failure x
  EIf _ expr1 expr2 expr3       -> failure x
  ELambda _ fulltype args stmts -> failure x
  ERand _ expr                  -> failure x
  ERandDist _ expr1 expr2       -> failure x
  EProbSamp _ expr1 stmts expr2 -> failure x
typecheckAddOp :: AddOp LineInfo -> TEnv -> FullType LineInfo -> Bool -> Bool -> TypeCheckResult
typecheckAddOp x tenv functype returned inloop = case x of
  Plus _  -> failure x
  Minus _ -> failure x
typecheckMulOp :: MulOp LineInfo -> TEnv -> FullType LineInfo -> Bool -> Bool -> TypeCheckResult
typecheckMulOp x tenv functype returned inloop = case x of
  Times _ -> failure x
  Div _   -> failure x
  Mod _   -> failure x
typecheckRelOp :: RelOp LineInfo -> TEnv -> FullType LineInfo -> Bool -> Bool -> TypeCheckResult
typecheckRelOp x tenv functype returned inloop = case x of
  LTH _ -> failure x
  LE _  -> failure x
  GTH _ -> failure x
  GE _  -> failure x
  EQU _ -> failure x
  NE _  -> failure x
transOrOp :: OrOp LineInfo -> TEnv -> FullType LineInfo -> Bool -> Bool -> TypeCheckResult
transOrOp x tenv functype returned inloop = case x of
  Or _ -> failure x
typecheckAndOp :: AndOp LineInfo -> TEnv -> FullType LineInfo -> Bool -> Bool -> TypeCheckResult
typecheckAndOp x tenv functype returned inloop = case x of
  And _ -> failure x
