module Tyche.TypeCheck where

import           Tyche.Abs
import           Tyche.Bool
import           Tyche.Converters
import           Tyche.Env
import           Tyche.ErrM
import           Tyche.Helpers
import           Tyche.Internal     (internals)
import           Tyche.Numerical
import           Tyche.Print
import           Tyche.TypeMatching
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
    passWithErrorHandle (typecheckStmt stmt tenv functype returned inloop) typecheckStmts stmts Nothing
typecheckProgram :: Program LineInfo -> TypeCheckResult
typecheckProgram x = case x of
  Program _ stmts ->
    let
      defineInternals :: TEnv -> [(Ident, FullType LineInfo, Val)] -> TEnv
      defineInternals tenv [] = tenv
      defineInternals tenv ((ident, fulltype, _):ints) =
        defineInternals (extendTEnv tenv ident fulltype) ints
    in
      typecheckStmts stmts (defineInternals (\v -> Nothing) internals) voidT False False
typecheckArg :: Arg LineInfo -> TEnv -> FullType LineInfo -> Bool -> Bool -> TypeCheckResult
typecheckArg x tenv functype returned inloop = case x of
  Arg _ argmod ident fulltype -> Ok (voidT, tenv, functype, returned, inloop)
typecheckStmt :: Stmt LineInfo -> TEnv -> FullType LineInfo -> Bool -> Bool -> TypeCheckResult
typecheckStmt x tenv functype returned inloop = case x of
  Skip _ -> Ok (voidT, tenv, functype, returned, inloop)
  Break lineinfo ->
    if inloop then
      Ok (voidT, tenv, functype, returned, inloop)
    else
      Bad ("Break statement outside of the loop\n\tat Break Statement " ++ (lineInfoString lineinfo) ++ "\n")
  Continue lineinfo                            ->
    if inloop then
      Ok (voidT, tenv, functype, returned, inloop)
    else
      Bad ("Continue statement outside of the loop\n\tat Continue Statement " ++ (lineInfoString lineinfo) ++ "\n")
  Ret lineinfo expr ->
    case typecheckExpr expr tenv functype returned inloop of
      Bad str -> Bad (str ++ "\tat Return Statement " ++ (lineInfoString lineinfo) ++ "\n")
      Ok (exprtype, _, _, _, _) ->
        if matchFullType exprtype functype then
          Ok (voidT, tenv, functype, True, inloop)
        else
          Bad ("Return value type `" ++ (printTree exprtype) ++ "` doesn't match expected function return type `" ++ (printTree functype) ++ "`\n\tat Return Statement " ++ (lineInfoString lineinfo) ++ "\n")
  VarDef lineinfo ident fulltype expr ->
    case typecheckExpr expr tenv functype returned inloop of
      Bad str -> Bad (str ++ "\tat Variable Definition " ++ (lineInfoString lineinfo) ++ "\n")
      Ok (exprtype, _, _, _, _) ->
        if matchFullType exprtype fulltype then
          Ok (voidT, extendTEnv tenv ident fulltype, functype, returned, inloop)
        else
          Bad ("Unable to match types in variable definition. Expected `" ++ (printTree fulltype) ++ "`, got `" ++ (printTree exprtype) ++ "`\n\tat Variable Definition " ++ (lineInfoString lineinfo) ++ "\n")
  Ass lineinfo ident expr ->
    case tenv ident of
      Nothing -> Bad ("Assigning value to an undeclared variable `" ++ (printTree ident) ++ "`\n\tat Assignment Statement " ++ (lineInfoString lineinfo) ++ "\n")
      Just fulltype ->
        if isReadonly fulltype then
          Bad ("Assigning value to a readonly variable `" ++ (printTree ident) ++ "`\n\tat Assignment Statement " ++ (lineInfoString lineinfo) ++ "\n")
        else
          case typecheckExpr expr tenv functype returned inloop of
            Bad str -> Bad (str ++ "\tat Assignment Statement " ++ (lineInfoString lineinfo) ++ "\n")
            Ok (exprtype, _, _, _, _) ->
              if matchFullType exprtype fulltype then
                Ok (voidT, tenv, functype, returned, inloop)
              else
                Bad ("Unable to match types in assignment. Expected `" ++ (printTree fulltype) ++ "`, got `" ++ (printTree exprtype) ++ "`\n\tat Assignment Statement " ++ (lineInfoString lineinfo) ++ "\n")
  FnDef lineinfo ident fulltype args stmts ->
    let
      tenvForInsideFunc = extendTEnv (extendTEnvArgs tenv args) ident (FullType lineinfo [] (Fun lineinfo (argsToArgTypes args) fulltype))
      tenvForAfterFunc = extendTEnv tenv ident (FullType lineinfo [] (Fun lineinfo (argsToArgTypes args) fulltype))
    in
      case typecheckStmts stmts tenvForInsideFunc fulltype False False of
        Bad str -> Bad (str ++ "\tat Function Definition " ++ (lineInfoString lineinfo) ++ "\n")
        Ok (_, _, _, returned', _) ->
          if returned' then
            Ok (voidT, tenvForAfterFunc, functype, returned, inloop)
          else
            Bad ("Function returns no value (return statement is missing)\n\tat Function Definition " ++ (lineInfoString lineinfo) ++ "\n")
  FnApp lineinfo expr exprs ->
    case typecheckExpr expr tenv functype returned inloop of
      Bad str -> Bad (str ++ "\tat Function Application " ++ (lineInfoString lineinfo) ++ "\n")
      Ok (ftype, _, _, _, _) ->
        case ftype of
          FullType _ _ (Fun _ argtypes fulltype) ->
            let
              checkArgTypesWithExprs [] [] = Ok (fulltype, tenv, functype, returned, inloop)
              checkArgTypesWithExprs [] e = Bad ("Too many arguments. Expected " ++ (show $ length argtypes) ++ ", got " ++ (show $ length exprs) ++ "\n\tat Function Application" ++ (lineInfoString lineinfo) ++ "\n")
              checkArgTypesWithExprs at [] = Bad ("Too few arguments. Expected " ++ (show $ length argtypes) ++ ", got " ++ (show $ length exprs) ++ "\n\tat Function Application" ++ (lineInfoString lineinfo) ++ "\n")
              checkArgTypesWithExprs (at:ats) (e:es) =
                case typecheckExpr e tenv functype returned inloop of
                  Bad str -> Bad (str ++ "\tat Function Application " ++ (lineInfoString lineinfo) ++ "\n")
                  Ok (etype, _, _, _, _) -> let expectedtype = argTypeToFullType at in
                    if matchFullType etype expectedtype then
                      checkArgTypesWithExprs ats es
                    else
                      Bad ("Can't match expected argument type `" ++ (printTree expectedtype) ++ "` with actual type `" ++ (printTree etype) ++ "`\n\tat Function Application " ++ (lineInfoString lineinfo) ++ "\n")
            in
              checkArgTypesWithExprs argtypes exprs
          otherwise -> Bad ("Expected function, got `" ++ (printTree ftype) ++ "`\n\tat Function Application " ++ (lineInfoString lineinfo) ++ "\n")
  Cond lineinfo expr stmts ->
    case typecheckExpr expr tenv functype returned inloop of
      Bad str -> Bad (str ++ "\tat If Statement " ++ (lineInfoString lineinfo) ++ "\n")
      Ok (exprtype, _, _, _, _) ->
        if matchFullType exprtype boolT then
          case typecheckStmts stmts tenv functype returned inloop of
            Bad str -> Bad (str ++ "\tat If Statement " ++ (lineInfoString lineinfo) ++ "\n")
            Ok (_, _, _, _, _) -> Ok (voidT, tenv, functype, returned, inloop)
        else
          Bad ("Expected expression type `" ++ (printTree boolT) ++ "`, got `" ++ (printTree exprtype) ++ "`\n\tat If Statement " ++ (lineInfoString lineinfo) ++ "\n")
  CondElse lineinfo expr stmts1 stmts2 ->
    case typecheckExpr expr tenv functype returned inloop of
      Bad str -> Bad (str ++ "\tat If-else Statement (inside the expression) " ++ (lineInfoString lineinfo) ++ "\n")
      Ok (exprtype, _, _, _, _) ->
        if matchFullType exprtype boolT then
          case typecheckStmts stmts1 tenv functype returned inloop of
            Bad str -> Bad (str ++ "\tat If-else Statement (`if` branch) " ++ (lineInfoString lineinfo) ++ "\n")
            Ok (_, _, _, returned1, _) ->
              case typecheckStmts stmts2 tenv functype returned inloop of
                Bad str -> Bad (str ++ "\tat If-else Statement (`else` branch) " ++ (lineInfoString lineinfo) ++ "\n")
                Ok (_, _, _, returned2, _) -> Ok (voidT, tenv, functype, returned || (returned1 && returned2), inloop)
        else
          Bad ("Expected expression type `" ++ (printTree boolT) ++ "`, got `" ++ (printTree exprtype) ++ "`\n\tat If-else Statement (inside the expression) " ++ (lineInfoString lineinfo) ++ "\n")
  While lineinfo expr stmts ->
    case typecheckExpr expr tenv functype returned inloop of
      Bad str -> Bad (str ++ "\tat While Loop (inside the expression) " ++ (lineInfoString lineinfo) ++ "\n")
      Ok (exprtype, _, _, _, _) ->
        if matchFullType exprtype boolT then
          case typecheckStmts stmts tenv functype returned True of
            Bad str -> Bad (str ++ "\tat While Loop (inside the loop) " ++ (lineInfoString lineinfo) ++ "\n")
            Ok (_, _, _, returned', _) -> Ok (voidT, tenv, functype, returned || returned', inloop)
        else
          Bad ("Expected expression type `" ++ (printTree boolT) ++ "`, got `" ++ (printTree exprtype) ++ "`\n\tat While Loop (inside the expression) " ++ (lineInfoString lineinfo) ++ "\n")
  ForList lineinfo ident expr stmts ->
    case typecheckExpr expr tenv functype returned inloop of
      Bad str -> Bad (str ++ "\tat For-each Statement (inside the expression) " ++ (lineInfoString lineinfo) ++ "\n")
      Ok (exprtype, _, _, _, _) ->
        if isArray exprtype || isList exprtype then
          let
            tenvWithLoopVariable = (extendTEnv tenv ident (addReadonly (elementType exprtype)))
          in
            case typecheckStmts stmts tenvWithLoopVariable functype returned True of
              Bad str -> Bad (str ++ "\tat For-each Statement (inside the loop) " ++ (lineInfoString lineinfo) ++ "\n")
              Ok (_, _, _, returned', _) -> Ok (voidT, tenv, functype, returned || returned', inloop)
        else
          Bad ("Expected an Array or a List, got `" ++ (printTree exprtype) ++ "`\n\tat For-each Statement (inside the expression) " ++ (lineInfoString lineinfo) ++ "\n")
  ForRange lineinfo ident expr1 expr2 stmts ->
    case typecheckExpr expr1 tenv functype returned inloop of
      Bad str -> Bad (str ++ "\tat For-range Statement (inside the first expression) " ++ (lineInfoString lineinfo) ++ "\n")
      Ok (expr1type, _, _, _, _) ->
        if matchFullType expr1type intT then
          case typecheckExpr expr2 tenv functype returned inloop of
            Bad str -> Bad (str ++ "\tat For-each Statement (inside the second expression) " ++ (lineInfoString lineinfo) ++ "\n")
            Ok (expr2type, _, _, _, _) ->
              if matchFullType expr2type intT then
                let
                  tenvWithLoopVariable = (extendTEnv tenv ident readonlyIntT)
                in
                  case typecheckStmts stmts tenvWithLoopVariable functype returned True of
                    Bad str -> Bad (str ++ "\tat For-each Statement (inside the loop) " ++ (lineInfoString lineinfo) ++ "\n")
                    Ok (_, _, _, returned', _) -> Ok (voidT, tenv, functype, returned || returned', inloop)
              else
                Bad ("Expected expression type `" ++ (printTree intT) ++ "`, got `" ++ (printTree expr2type) ++ "`\n\tat For-range Statement (inside the second expression) " ++ (lineInfoString lineinfo) ++ "\n")
        else
          Bad ("Expected expression type `" ++ (printTree intT) ++ "`, got `" ++ (printTree expr1type) ++ "`\n\tat For-range Statement (inside the first expression) " ++ (lineInfoString lineinfo) ++ "\n")
typecheckType :: Type LineInfo -> TEnv -> FullType LineInfo -> Bool -> Bool -> TypeCheckResult
typecheckType x tenv functype returned inloop = case x of
  Int _ -> Ok (voidT, tenv, functype, returned, inloop)
  Str _ -> Ok (voidT, tenv, functype, returned, inloop)
  Bool _ -> Ok (voidT, tenv, functype, returned, inloop)
  Void _ -> Ok (voidT, tenv, functype, returned, inloop)
  Float _ -> Ok (voidT, tenv, functype, returned, inloop)
  List lineinfo fulltype -> passWithErrorHandle (typecheckFullType fulltype tenv functype returned inloop) typecheckNothing lineinfo Nothing
  Array lineinfo fulltype -> passWithErrorHandle (typecheckFullType fulltype tenv functype returned inloop) typecheckNothing lineinfo Nothing
  Fun lineinfo argtypes fulltype ->
    case typecheckArgTypes argtypes tenv functype returned inloop of
      Bad str -> Bad (str ++ "\tat Function Type " ++ (lineInfoString lineinfo) ++ "\n")
      Ok (_, _, _, _, _) ->
        case typecheckFullType fulltype tenv functype returned inloop of
          Bad str -> Bad (str ++ "\tat Function Type " ++ (lineInfoString lineinfo) ++ "\n")
          Ok (_, _, _, _, _) -> Ok (voidT, tenv, functype, returned, inloop)
typecheckArgTypes :: [ArgType LineInfo] -> TEnv -> FullType LineInfo -> Bool -> Bool -> TypeCheckResult
typecheckArgTypes x tenv functype returned inloop = case x of
  [] -> Ok (voidT, tenv, functype, returned, inloop)
  argtype:argtypes ->
    passWithErrorHandle (typecheckArgType argtype tenv functype returned inloop) typecheckArgTypes argtypes Nothing
typecheckArgType :: ArgType LineInfo -> TEnv -> FullType LineInfo -> Bool -> Bool -> TypeCheckResult
typecheckArgType x tenv functype returned inloop = case x of
  ArgType lineinfo argmod fulltype ->
    case typecheckArgMod argmod tenv functype returned inloop of
      Bad str -> Bad (str ++ "\tat Argument Type " ++ (lineInfoString lineinfo) ++ "\n")
      Ok (_, _, _, _, _) ->
        case typecheckFullType fulltype tenv functype returned inloop of
          Bad str -> Bad (str ++ "\tat Argument Type " ++ (lineInfoString lineinfo) ++ "\n")
          Ok (_, _, _, _, _) -> Ok (voidT, tenv, functype, returned, inloop)
typecheckFullType :: FullType LineInfo -> TEnv -> FullType LineInfo -> Bool -> Bool -> TypeCheckResult
typecheckFullType x tenv functype returned inloop = case x of
  FullType lineinfo typemods type_ ->
    case typecheckTypeMods typemods tenv functype returned inloop of
      Bad str -> Bad (str ++ "\tat Full Type (type modifiers) " ++ (lineInfoString lineinfo) ++ "\n")
      Ok (_, _, _, _, _) ->
        case typecheckType type_ tenv functype returned inloop of
          Bad str -> Bad (str ++ "\tat Full Type (type) " ++ (lineInfoString lineinfo) ++ "\n")
          Ok (_, _, _, _, _) -> Ok (voidT, tenv, functype, returned, inloop)
typecheckArgMod :: ArgMod LineInfo -> TEnv -> FullType LineInfo -> Bool -> Bool -> TypeCheckResult
typecheckArgMod x tenv functype returned inloop = case x of
  AModVar _   -> Ok (voidT, tenv, functype, returned, inloop)
  AModVal _   -> Ok (voidT, tenv, functype, returned, inloop)
  AModInOut _ -> Ok (voidT, tenv, functype, returned, inloop)
typecheckTypeMods :: [TypeMod LineInfo] -> TEnv -> FullType LineInfo -> Bool -> Bool -> TypeCheckResult
typecheckTypeMods x tenv functype returned inloop = case x of
  [] -> Ok (voidT, tenv, functype, returned, inloop)
  typemod:typemods ->
    passWithErrorHandle (typecheckTypeMod typemod tenv functype returned inloop) typecheckTypeMods typemods Nothing
typecheckTypeMod :: TypeMod LineInfo -> TEnv -> FullType LineInfo -> Bool -> Bool -> TypeCheckResult
typecheckTypeMod x tenv functype returned inloop = case x of
  TModReadonly _ -> Ok (voidT, tenv, functype, returned, inloop)
typecheckExpr :: Expr LineInfo -> TEnv -> FullType LineInfo -> Bool -> Bool -> TypeCheckResult
typecheckExpr x tenv functype returned inloop = case x of
  ELitVoid lineinfo -> Ok (voidT, tenv, functype, returned, inloop)
  EVar lineinfo ident ->
    case tenv ident of
      Nothing -> Bad ("Undefined variable `" ++ (printTree ident) ++ "`\n\tat Variable Expression " ++ (lineInfoString lineinfo) ++ "\n")
      Just identtype -> Ok (identtype, tenv, functype, returned, inloop)
  ELitInt lineinfo integer -> Ok (intT, tenv, functype, returned, inloop)
  ELitTrue lineinfo -> Ok (boolT, tenv, functype, returned, inloop)
  ELitFalse lineinfo -> Ok (boolT, tenv, functype, returned, inloop)
  EString lineinfo string -> Ok (stringT, tenv, functype, returned, inloop)
  ELitFloat lineinfo double -> Ok (floatT, tenv, functype, returned, inloop)
  EEmpList lineinfo fulltype -> Ok (listT fulltype, tenv, functype, returned, inloop)
  EEmpArray lineinfo fulltype -> Ok (arrayT fulltype, tenv, functype, returned, inloop)
  EApp lineinfo expr exprs ->
    case typecheckExpr expr tenv functype returned inloop of
      Bad str -> Bad (str ++ "\tat Function Application " ++ (lineInfoString lineinfo) ++ "\n")
      Ok (ftype, _, _, _, _) ->
        case ftype of
          FullType _ _ (Fun _ argtypes fulltype) ->
            let
              checkArgTypesWithExprs [] [] = Ok (fulltype, tenv, functype, returned, inloop)
              checkArgTypesWithExprs [] e = Bad ("Too many arguments. Expected " ++ (show $ length argtypes) ++ ", got " ++ (show $ length exprs) ++ "\n\tat Function Application" ++ (lineInfoString lineinfo) ++ "\n")
              checkArgTypesWithExprs at [] = Bad ("Too few arguments. Expected " ++ (show $ length argtypes) ++ ", got " ++ (show $ length exprs) ++ "\n\tat Function Application" ++ (lineInfoString lineinfo) ++ "\n")
              checkArgTypesWithExprs (at:ats) (e:es) =
                case typecheckExpr e tenv functype returned inloop of
                  Bad str -> Bad (str ++ "\tat Function Application " ++ (lineInfoString lineinfo) ++ "\n")
                  Ok (etype, _, _, _, _) -> let expectedtype = argTypeToFullType at in
                    if matchFullType etype expectedtype then
                      checkArgTypesWithExprs ats es
                    else
                      Bad ("Can't match expected argument type `" ++ (printTree expectedtype) ++ "` with actual type `" ++ (printTree etype) ++ "`\n\tat Function Application " ++ (lineInfoString lineinfo) ++ "\n")
            in
              checkArgTypesWithExprs argtypes exprs
          otherwise -> Bad ("Expected function, got `" ++ (printTree ftype) ++ "`\n\tat Function Application " ++ (lineInfoString lineinfo) ++ "\n")
  Neg lineinfo expr ->
    case typecheckExpr expr tenv functype returned inloop of
      Bad str -> Bad (str ++ "\tat Negation Expression " ++ (lineInfoString lineinfo) ++ "\n")
      Ok (exprtype, _, _, _, _) ->
        if isNumeric exprtype then
          Ok (exprtype, tenv, functype, returned, inloop)
        else
          Bad ("Can't apply negation to the element of type `" ++ (printTree exprtype) ++ "`\n\tat Negation Expression " ++ (lineInfoString lineinfo) ++ "\n")
  Not lineinfo expr ->
    case typecheckExpr expr tenv functype returned inloop of
      Bad str -> Bad (str ++ "\tat Logical Negation " ++ (lineInfoString lineinfo) ++ "\n")
      Ok (exprtype, _, _, _, _) ->
        if matchFullType exprtype boolT then
          Ok (exprtype, tenv, functype, returned, inloop)
        else
          Bad ("Can't apply logical negation to the element of type `" ++ (printTree exprtype) ++ "`\n\tat Logical Negation " ++ (lineInfoString lineinfo) ++ "\n")
  ECons lineinfo expr1 expr2 ->
    case typecheckExpr expr1 tenv functype returned inloop of
      Bad str -> Bad (str ++ "\tat Cons Expression (element expression) " ++ (lineInfoString lineinfo) ++ "\n")
      Ok (expr1type, _, _, _, _) ->
        case typecheckExpr expr2 tenv functype returned inloop of
          Bad str -> Bad (str ++ "\tat Cons Expression (list expression) " ++ (lineInfoString lineinfo) ++ "\n")
          Ok (expr2type, _, _, _, _) ->
            if isList expr2type then
              if matchFullType expr1type (listElementType expr2type) then
                Ok (expr2type, tenv, functype, returned, inloop)
              else
                Bad ("Type `" ++ (printTree expr1type) ++ "` is not a valid element type for list `" ++ (printTree expr2type) ++ "`\n\tat Cons Expression " ++ (lineInfoString lineinfo) ++ "\n")
            else
              Bad ("Expected list, got `" ++ (printTree expr2type) ++ "`\n\tat Cons Expression " ++ (lineInfoString lineinfo) ++ "\n")
  EMul lineinfo expr1 mulop expr2 ->
    case typecheckExpr expr1 tenv functype returned inloop of
      Bad str -> Bad (str ++ "\tat Multiplication (left argument) " ++ (lineInfoString lineinfo) ++ "\n")
      Ok (expr1type, _, _, _, _) ->
        if isNumeric expr1type then
          case typecheckExpr expr2 tenv functype returned inloop of
            Bad str -> Bad (str ++ "\tat Multiplication (right argument) " ++ (lineInfoString lineinfo) ++ "\n")
            Ok (expr2type, _, _, _, _) ->
              if isNumeric expr2type then
                Ok (unifyNumericTypes expr1type expr2type, tenv, functype, returned, inloop)
              else
                Bad ("Can't multiply non-numeric expression of type `" ++ (printTree expr2type) ++ "`\n\tat Multiplication (right argument) " ++ (lineInfoString lineinfo) ++ "\n")
        else
          Bad ("Can't multiply non-numeric expression of type `" ++ (printTree expr1type) ++ "`\n\tat Multiplication (left argument) " ++ (lineInfoString lineinfo) ++ "\n")
  EAdd lineinfo expr1 addop expr2      ->
    case typecheckExpr expr1 tenv functype returned inloop of
      Bad str -> Bad (str ++ "\tat Addition (left argument) " ++ (lineInfoString lineinfo) ++ "\n")
      Ok (expr1type, _, _, _, _) ->
        if isNumeric expr1type then
          case typecheckExpr expr2 tenv functype returned inloop of
            Bad str -> Bad (str ++ "\tat Addition (right argument) " ++ (lineInfoString lineinfo) ++ "\n")
            Ok (expr2type, _, _, _, _) ->
              if isNumeric expr2type then
                Ok (unifyNumericTypes expr1type expr2type, tenv, functype, returned, inloop)
              else
                Bad ("Can't add non-numeric expression of type `" ++ (printTree expr2type) ++ "`\n\tat Addition (right argument) " ++ (lineInfoString lineinfo) ++ "\n")
        else
          Bad ("Can't add non-numeric expression of type `" ++ (printTree expr1type) ++ "`\n\tat Addition (left argument) " ++ (lineInfoString lineinfo) ++ "\n")
  ERel lineinfo expr1 relop expr2 ->
    case typecheckExpr expr1 tenv functype returned inloop of
      Bad str -> Bad (str ++ "\tat Comparison (left argument) " ++ (lineInfoString lineinfo) ++ "\n")
      Ok (expr1type, _, _, _, _) ->
        if isNumeric expr1type then
          case typecheckExpr expr2 tenv functype returned inloop of
            Bad str -> Bad (str ++ "\tat Comparison (right argument) " ++ (lineInfoString lineinfo) ++ "\n")
            Ok (expr2type, _, _, _, _) ->
              if isNumeric expr2type then
                Ok (boolT, tenv, functype, returned, inloop)
              else
                Bad ("Can't compare non-numeric expression of type `" ++ (printTree expr2type) ++ "`\n\tat Comparison (right argument) " ++ (lineInfoString lineinfo) ++ "\n")
        else
          Bad ("Can't compare non-numeric expression of type `" ++ (printTree expr1type) ++ "`\n\tat Comparison (left argument) " ++ (lineInfoString lineinfo) ++ "\n")
  EAnd lineinfo expr1 andop expr2 ->
    case typecheckExpr expr1 tenv functype returned inloop of
      Bad str -> Bad (str ++ "\tat Conjunction (left argument) " ++ (lineInfoString lineinfo) ++ "\n")
      Ok (expr1type, _, _, _, _) ->
        if isBool expr1type then
          case typecheckExpr expr2 tenv functype returned inloop of
            Bad str -> Bad (str ++ "\tat Conjunction (right argument) " ++ (lineInfoString lineinfo) ++ "\n")
            Ok (expr2type, _, _, _, _) ->
              if isBool expr2type then
                Ok (boolT, tenv, functype, returned, inloop)
              else
                Bad ("Can't apply `and` to non-boolean expression of type `" ++ (printTree expr2type) ++ "`\n\tat Conjunction (right argument) " ++ (lineInfoString lineinfo) ++ "\n")
        else
          Bad ("Can't apply `and` to non-boolean expression of type `" ++ (printTree expr1type) ++ "`\n\tat Conjunction (left argument) " ++ (lineInfoString lineinfo) ++ "\n")
  EOr lineinfo expr1 orop expr2 ->
    case typecheckExpr expr1 tenv functype returned inloop of
      Bad str -> Bad (str ++ "\tat Alternative (left argument) " ++ (lineInfoString lineinfo) ++ "\n")
      Ok (expr1type, _, _, _, _) ->
        if isBool expr1type then
          case typecheckExpr expr2 tenv functype returned inloop of
            Bad str -> Bad (str ++ "\tat Alternative (right argument) " ++ (lineInfoString lineinfo) ++ "\n")
            Ok (expr2type, _, _, _, _) ->
              if isBool expr2type then
                Ok (boolT, tenv, functype, returned, inloop)
              else
                Bad ("Can't apply `or` to non-boolean expression of type `" ++ (printTree expr2type) ++ "`\n\tat Alternative (right argument) " ++ (lineInfoString lineinfo) ++ "\n")
        else
          Bad ("Can't apply `or` to non-boolean expression of type `" ++ (printTree expr1type) ++ "`\n\tat Alternative (left argument) " ++ (lineInfoString lineinfo) ++ "\n")
  EList lineinfo exprs ->
    let
      checkExprs (e:nil) =
        case typecheckExpr e tenv functype returned inloop of
          Bad str -> Bad (str ++ "\tat List Expression " ++ (lineInfoString lineinfo) ++ "\n")
          Ok (etype, _, _, _, _) -> Ok (etype, tenv, functype, returned, inloop)
      checkExprs (e:es) =
        case typecheckExpr e tenv functype returned inloop of
          Bad str -> Bad (str ++ "\tat List Expression " ++ (lineInfoString lineinfo) ++ "\n")
          Ok (etype, _, _, _, _) ->
            case checkExprs es of
              Bad str -> Bad (str ++ "\tat List Expression " ++ (lineInfoString lineinfo) ++ "\n")
              Ok (e2type, _, _, _, _) ->
                if matchFullType etype e2type then
                  Ok (etype, tenv, functype, returned, inloop)
                else
                  Bad ("List expression elements have different types: `" ++ (printTree etype) ++ "` and `" ++ (printTree e2type) ++ "`\n\tat List Expression " ++ (lineInfoString lineinfo) ++ "\n")
    in
      case checkExprs exprs of
        Bad str -> Bad str
        Ok (listelementtype, _, _, _, _) -> Ok (listT listelementtype, tenv, functype, returned, inloop)
  EArr lineinfo exprs ->
    let
      checkExprs (e:nil) =
        case typecheckExpr e tenv functype returned inloop of
          Bad str -> Bad (str ++ "\tat Array Expression " ++ (lineInfoString lineinfo) ++ "\n")
          Ok (etype, _, _, _, _) -> Ok (etype, tenv, functype, returned, inloop)
      checkExprs (e:es) =
        case typecheckExpr e tenv functype returned inloop of
          Bad str -> Bad (str ++ "\tat Array Expression " ++ (lineInfoString lineinfo) ++ "\n")
          Ok (etype, _, _, _, _) ->
            case checkExprs es of
              Bad str -> Bad (str ++ "\tat Array Expression " ++ (lineInfoString lineinfo) ++ "\n")
              Ok (e2type, _, _, _, _) ->
                if matchFullType etype e2type then
                  Ok (etype, tenv, functype, returned, inloop)
                else
                  Bad ("Array expression elements have different types: `" ++ (printTree etype) ++ "` and `" ++ (printTree e2type) ++ "`\n\tat Array Expression " ++ (lineInfoString lineinfo) ++ "\n")
    in
      case checkExprs exprs of
        Bad str -> Bad str
        Ok (arrayelementtype, _, _, _, _) -> Ok (arrayT arrayelementtype, tenv, functype, returned, inloop)
  EArrSize lineinfo expr1 expr2 fulltype ->
    case typecheckExpr expr1 tenv functype returned inloop of
      Bad str -> Bad (str ++ "\tat Default Value Array Expression " ++ (lineInfoString lineinfo) ++ "\n")
      Ok (exprtype1, _, _, _, _) ->
        if isInt exprtype1 then
          case typecheckExpr expr2 tenv functype returned inloop of
            Bad str -> Bad (str ++ "\tat Default Value Array Expression " ++ (lineInfoString lineinfo) ++ "\n")
            Ok (expr2type, _, _, _, _) ->
              if matchFullType expr2type fulltype then
                Ok (arrayT fulltype, tenv, functype, returned, inloop)
              else
                Bad ("Array value initializer type `" ++ (printTree expr2type) ++ "` doesn't match array value type `" ++ (printTree fulltype) ++ "`\n\tat Default Value Array Expression " ++ (lineInfoString lineinfo) ++ "\n")
        else
          Bad ("Expected expression type `" ++ (printTree intT) ++ "`, got `" ++ (printTree exprtype1) ++ "`\n\tat Default Value Array Expression " ++ (lineInfoString lineinfo) ++ "\n")
  EArrApp lineinfo expr1 expr2 ->
    case typecheckExpr expr1 tenv functype returned inloop of
      Bad str -> Bad (str ++ "\tat Array Application (array expression) " ++ (lineInfoString lineinfo) ++ "\n")
      Ok (expr1type, _, _, _, _) ->
        if isArray expr1type then
          case typecheckExpr expr2 tenv functype returned inloop of
            Bad str -> Bad (str ++ "\tat Array Application (index expression) " ++ (lineInfoString lineinfo) ++ "\n")
            Ok (expr2type, _, _, _, _) ->
             if isInt expr2type then
               Ok (arrayElementType expr1type, tenv, functype, returned, inloop)
             else
               Bad ("Expected `" ++ (printTree intT) ++ "`, got `" ++ (printTree expr2type) ++ "`\n\tat Array Application (index expression) " ++ (lineInfoString lineinfo) ++ "\n")
        else
          Bad ("Expected array, got `" ++ (printTree expr1type) ++ "`\n\tat Array Application (array expression) " ++ (lineInfoString lineinfo) ++ "\n")
  EIf lineinfo expr1 expr2 expr3 ->
    case typecheckExpr expr1 tenv functype returned inloop of
      Bad str -> Bad (str ++ "\tat If-else Expression (condition) " ++ (lineInfoString lineinfo) ++ "\n")
      Ok (expr1type, _, _, _, _) ->
        if matchFullType expr1type boolT then
          case typecheckExpr expr2 tenv functype returned inloop of
            Bad str -> Bad (str ++ "\tat If-else Expression (if branch) " ++ (lineInfoString lineinfo) ++ "\n")
            Ok (expr2type, _, _, _, _) ->
              case typecheckExpr expr3 tenv functype returned inloop of
                Bad str -> Bad (str ++ "\tat If-else Expression (else branch) " ++ (lineInfoString lineinfo) ++ "\n")
                Ok (expr3type, _, _, _, _) ->
                  if matchFullType expr2type expr3type then
                    Ok (expr2type, tenv, functype, returned, inloop)
                  else
                    Bad ("If branches have different types: `" ++ (printTree expr2type) ++ "` and `" ++ (printTree expr3type) ++ "`\n\tat If-else Expression " ++ (lineInfoString lineinfo) ++ "\n")
        else
          Bad ("Expected `" ++ (printTree boolT) ++ "`, got `" ++ (printTree expr1type) ++ "`\n\tat If-else Expression (condition) " ++ (lineInfoString lineinfo) ++ "\n")
  ELambda lineinfo fulltype args stmts ->
    let
      tenvInsideFunc = extendTEnvArgs tenv args
    in
      case typecheckStmts stmts tenvInsideFunc fulltype False False of
        Bad str -> Bad (str ++ "\tat Lambda expression (inside the function body) " ++ (lineInfoString lineinfo) ++ "\n")
        Ok (_, _, _, returned', _) ->
          if returned' then
            Ok (FullType lineinfo [] (Fun lineinfo (argsToArgTypes args) fulltype), tenv, functype, returned, inloop)
          else
            Bad ("Function is missing return statement\n\tat Lambda expression (inside the function body) " ++ (lineInfoString lineinfo) ++ "\n")
  ERand lineinfo expr ->
    case typecheckExpr expr tenv functype returned inloop of
      Bad str -> Bad (str ++ "\tat Random Expression " ++ (lineInfoString lineinfo) ++ "\n")
      Ok (exprtype, _, _, _, _) ->
        if isArray exprtype || isList exprtype then
          Ok (elementType exprtype, tenv, functype, returned, inloop)
        else
          Bad ("Expected an Array or a List, got `" ++ (printTree exprtype) ++ "`\n\tat Random Expression " ++ (lineInfoString lineinfo) ++ "\n")
  ERandDist lineinfo expr1 expr2 ->
    case typecheckExpr expr1 tenv functype returned inloop of
      Bad str -> Bad (str ++ "\tat Random with Distribution " ++ (lineInfoString lineinfo) ++ "\n")
      Ok (expr1type, _, _, _, _) ->
        if isArray expr1type || isList expr1type then
          case typecheckExpr expr2 tenv functype returned inloop of
            Bad str -> Bad (str ++ "\tat Random with Distribution " ++ (lineInfoString lineinfo) ++ "\n")
            Ok (expr2type, _, _, _, _) ->
              if isArray expr2type || isList expr2type then
                if matchFullType (elementType expr2type) floatT then
                  Ok (elementType expr1type, tenv, functype, returned, inloop)
                else
                  Bad ("Expected element type `" ++ (printTree floatT) ++ "`, got `" ++ (printTree (elementType expr2type)) ++ "`\n\tat Random with Distribution " ++ (lineInfoString lineinfo) ++ "\n")
              else
                Bad ("Expected an Array or a List, got `" ++ (printTree expr2type) ++ "`\n\tat Random with Distribution " ++ (lineInfoString lineinfo) ++ "\n")
        else
          Bad ("Expected an Array or a List, got `" ++ (printTree expr1type) ++ "`\n\tat Random with Distribution " ++ (lineInfoString lineinfo) ++ "\n")
  EProbSamp lineinfo expr1 stmts expr2 ->
    case typecheckExpr expr1 tenv functype returned inloop of
      Bad str -> Bad (str ++ "\tat Sampled Probability (number of samples expression) " ++ (lineInfoString lineinfo) ++ "\n")
      Ok (expr1type, _, _, _, _) ->
        if matchFullType expr1type intT then
          case typecheckStmts stmts tenv voidT False False of
            Bad str -> Bad (str ++ "\tat Sampled Probability (statement block) " ++ (lineInfoString lineinfo) ++ "\n")
            Ok (_, tenv', _, _, _) ->
              case typecheckExpr expr2 tenv' functype returned inloop of
                Bad str -> Bad (str ++ "\tat Sampled Probability (state condition) " ++ (lineInfoString lineinfo) ++ "\n")
                Ok (expr2type, _, _, _, _) ->
                  if matchFullType expr2type boolT then
                    Ok (floatT, tenv, functype, returned, inloop)
                  else
                    Bad ("Expected `" ++ (printTree boolT) ++ "`, got `" ++ (printTree expr2type) ++ "`\n\tat Sampled Probability (state condition) " ++ (lineInfoString lineinfo) ++ "\n")
        else
          Bad ("Expected `" ++ (printTree intT) ++ "`, got `" ++ (printTree expr1type) ++ "`\n\tat Sampled Probability (number of samples expression) " ++ (lineInfoString lineinfo) ++ "\n")
typecheckAddOp :: AddOp LineInfo -> TEnv -> FullType LineInfo -> Bool -> Bool -> TypeCheckResult
typecheckAddOp x tenv functype returned inloop = case x of
  Plus _  -> Ok (voidT, tenv, functype, returned, inloop)
  Minus _ -> Ok (voidT, tenv, functype, returned, inloop)
typecheckMulOp :: MulOp LineInfo -> TEnv -> FullType LineInfo -> Bool -> Bool -> TypeCheckResult
typecheckMulOp x tenv functype returned inloop = case x of
  Times _ -> Ok (voidT, tenv, functype, returned, inloop)
  Div _   -> Ok (voidT, tenv, functype, returned, inloop)
  Mod _   -> Ok (voidT, tenv, functype, returned, inloop)
typecheckRelOp :: RelOp LineInfo -> TEnv -> FullType LineInfo -> Bool -> Bool -> TypeCheckResult
typecheckRelOp x tenv functype returned inloop = case x of
  LTH _ -> Ok (voidT, tenv, functype, returned, inloop)
  LE _  -> Ok (voidT, tenv, functype, returned, inloop)
  GTH _ -> Ok (voidT, tenv, functype, returned, inloop)
  GE _  -> Ok (voidT, tenv, functype, returned, inloop)
  EQU _ -> Ok (voidT, tenv, functype, returned, inloop)
  NE _  -> Ok (voidT, tenv, functype, returned, inloop)
transOrOp :: OrOp LineInfo -> TEnv -> FullType LineInfo -> Bool -> Bool -> TypeCheckResult
transOrOp x tenv functype returned inloop = case x of
  Or _ -> Ok (voidT, tenv, functype, returned, inloop)
typecheckAndOp :: AndOp LineInfo -> TEnv -> FullType LineInfo -> Bool -> Bool -> TypeCheckResult
typecheckAndOp x tenv functype returned inloop = case x of
  And _ -> Ok (voidT, tenv, functype, returned, inloop)
