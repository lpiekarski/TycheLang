module Tyche.TypeCheck where

import Tyche.Abs
import Tyche.ErrM
import Tyche.Print
import Tyche.Types

typecheckIdent :: Ident -> TEnv -> Scope -> TypeCheckResult
typecheckIdent x te scope =
  case x of
    Ident string ->
      Ok (te, Nothing, scope)
typecheckProgram :: Program (Maybe (Int, Int)) -> TypeCheckResult
typecheckProgram x =
  case x of
    Program _ stmt ->
      typecheckStmt stmt (\v -> Nothing) (Nothing, False, 0)
typecheckArg :: Arg (Maybe (Int, Int)) -> TEnv -> Scope -> TypeCheckResult
typecheckArg x te scope =
  case x of
    Arg _ argmod fullident fulltype ->
      Ok (te, Nothing, scope)
typecheckFullIdent :: FullIdent (Maybe (Int, Int)) -> TEnv -> Scope -> TypeCheckResult
typecheckFullIdent x te scope =
  case x of
    FullIdent _ ident ->
      Ok (te, Nothing, scope)
    AnonIdent _ ->
      Ok (te, Nothing, scope)
typecheckStmt :: Stmt (Maybe (Int, Int)) -> TEnv -> Scope -> TypeCheckResult
typecheckStmt x te (scope@(funcType, ret, loops)) =
  case x of
    Skip _ ->
      Ok (te, Nothing, (funcType, ret, loops))
    Break lineInfo -> 
      if loops > 0 then
        Ok (te, Nothing, scope)
      else
        Bad ("at " ++ (lineInfoString lineInfo) ++ " Break statement outside of the loop")
    Continue lineInfo ->
      if loops > 0 then
        Ok (te, Nothing, scope)
      else
        Bad ("at " ++ (lineInfoString lineInfo) ++ " Continue statement outside of the loop")
    Ret lineInfo expr ->
      case typecheckExpr expr te scope of
        Bad str ->
          Bad str
        Ok (_, mft, _) ->
          case (mft, funcType) of
            (Nothing, Nothing) ->
              Ok (te, Nothing, scope)
            (Just ft, Just funcft) ->
              if matchFullType ft funcft then
                Ok (te, Nothing, (funcType, True, loops))
              else
                Bad ("at " ++ (lineInfoString lineInfo) ++ " Function of type " ++ (printTree funcft) ++ " returns a value of type " ++ (printTree ft))
            (Nothing, Just funcft) ->
              Bad ("at " ++ (lineInfoString lineInfo) ++ " Function returns wrong type. Got Nothing, expected " ++ (printTree funcft));
            (Just ft, Nothing) ->
              Ok (te, Nothing, (funcType, True, loops))
    VRet lineInfo ->
      case funcType of
        Nothing ->
          Bad ("at " ++ (lineInfoString lineInfo) ++ " Return statement outside of function")
        Just ft ->
          if isVoid ft then
            Ok (te, Nothing, (funcType, True, loops))
          else
            Bad ("at " ++ (lineInfoString lineInfo) ++ " Return statement without value. Expected value type: " ++ (printTree ft))
    VarDef lineInfo fullident fulltype expr ->
      case typecheckExpr expr te scope of
        Bad str ->
          Bad str
        Ok (te2, res, _) ->
          case res of
            Nothing ->
              Bad ("at " ++ (lineInfoString lineInfo) ++ " Expression has no type")
            Just ft ->
              if (matchFullType fulltype ft) then
                case fullident of
                  FullIdent _ ident ->
                    Ok (extendFunc te2 ident (Just fulltype), Nothing, scope)
                  AnonIdent _ ->
                    Ok (te2, Nothing, scope)
              else
                Bad ("at " ++ (lineInfoString lineInfo) ++ " Cannot assign type " ++ (printTree ft) ++ " to type " ++ (printTree fulltype))
    Ass lineInfo ident expr ->
      case typecheckExpr expr te scope of
        Bad str ->
          Bad str
        Ok (te2, res, _) ->
          case res of
            Nothing ->
              Bad ("at " ++ (lineInfoString lineInfo) ++ " Expression has no type")
            Just ft ->
              let mfulltype = te ident in
                case mfulltype of
                  Nothing ->
                    Bad ("at " ++ (lineInfoString lineInfo) ++ " Undefined variable " ++ (printTree ident))
                  Just fulltype ->
                    if isReadonly fulltype then
                      Bad ("at " ++ (lineInfoString lineInfo) ++ " Cannot assign value to readonly variable")
                    else if (matchFullType fulltype ft) then
                      Ok (te2, Nothing, scope)
                    else
                      Bad ("at " ++ (lineInfoString lineInfo) ++ " Cannot assign type " ++ (printTree ft) ++ " to type " ++ (printTree fulltype))
    FnDef lineInfo fullident fulltype args stmt ->
      case fullident of
        FullIdent _ ident ->
          case typecheckStmt stmt (extendFunc (extendFuncArgs te args) ident (Just (FullType lineInfo [] (Fun lineInfo (argsToArgTypes args) fulltype)))) (Just fulltype, False, 0) of
            Bad str ->
              Bad str
            Ok (_, _, (_, ret, _)) ->
              if ret then
                Ok (extendFunc te ident (Just (FullType lineInfo [] (Fun lineInfo (argsToArgTypes args) fulltype))), Nothing, scope)
              else
                Bad ("at " ++ (lineInfoString lineInfo) ++ " Function is missing return statement")
        AnonIdent _ ->
          case typecheckStmt stmt (extendFuncArgs te args) (Just fulltype, False, 0) of
            Bad str ->
              Bad str
            Ok (_, _, (_, ret, _)) ->
              if ret then
                Ok (te, Nothing, scope)
              else
                Bad ("at " ++ (lineInfoString lineInfo) ++ " Function is missing return statement")
    Cond lineInfo expr stmt ->
      case typecheckExpr expr te scope of
        Bad str ->
          Bad str
        Ok (_, res, _) ->
          case res of
            Nothing ->
              Bad ("at " ++ (lineInfoString lineInfo) ++ " Expression has no type")
            Just ft ->
              if isBool ft then
                case typecheckStmt stmt te scope of
                  Bad str ->
                    Bad str
                  Ok (_, _, _) ->
                    Ok (te, Nothing, scope)
              else
                Bad ("at " ++ (lineInfoString lineInfo) ++ " Expected expression type Bool")
    CondElse lineInfo expr stmt1 stmt2 ->
      case typecheckExpr expr te scope of
        Bad str ->
          Bad str
        Ok (_, res, _) ->
          case res of
            Nothing ->
              Bad ("at " ++ (lineInfoString lineInfo) ++ " Expression has no type")
            Just ft ->
              if isBool ft then
                case typecheckStmt stmt1 te scope of
                  Bad str ->
                    Bad str
                  Ok (_, _, (_, ret1, _)) ->
                    case typecheckStmt stmt2 te scope of
                      Bad str ->
                        Bad str
                      Ok (_, _, (_, ret2, _)) ->
                        Ok (te, Nothing, (funcType, ret || (ret1 && ret2), loops))
              else
                Bad ("at " ++ (lineInfoString lineInfo) ++ " Expected expression type Bool")
    While lineInfo expr stmt ->
      case typecheckExpr expr te scope of
        Bad str ->
          Bad str
        Ok (_, res, _) ->
          case res of
            Nothing ->
              Bad ("at " ++ (lineInfoString lineInfo) ++ " Expression has no type")
            Just ft ->
              if isBool ft then
                case typecheckStmt stmt te (funcType, ret, loops + 1) of
                  Bad str ->
                    Bad str
                  Ok (_, _, (_, ret1, _)) ->
                    Ok (te, Nothing, (funcType, ret || ret1, loops))
              else
                Bad ("at " ++ (lineInfoString lineInfo) ++ " Expression has type " ++ (printTree ft) ++". Expected type Bool")
    ForList lineInfo ident expr stmt ->
      case typecheckExpr expr te scope of
        Bad str ->
          Bad str
        Ok (_, res, _) ->
          case res of
            Nothing ->
              Bad ("at " ++ (lineInfoString lineInfo) ++ " Expression has no type. Expected Array or List")
            Just ft ->
              if isArray ft then
                case arrayElementType ft of
                  Nothing ->
                    Bad ("at " ++ (lineInfoString lineInfo) ++ " Expression is not an Array")
                  Just (FullType _ _ t) ->
                    case typecheckStmt stmt (extendFunc te ident (Just (FullType lineInfo [TModReadonly lineInfo] t))) (funcType, ret, loops + 1) of
                      Bad str ->
                        Bad str
                      Ok (_, _, (_, ret1, _)) ->
                        Ok (te, Nothing, (funcType, ret || ret1, loops))
              else if isList ft then
                case listElementType ft of
                  Nothing ->
                    Bad ("at " ++ (lineInfoString lineInfo) ++ " Expression is not a List")
                  Just (FullType _ _ t) ->
                    case typecheckStmt stmt (extendFunc te ident (Just (FullType lineInfo [TModReadonly lineInfo] t))) (funcType, ret, loops + 1) of
                      Bad str ->
                        Bad str
                      Ok (_, _, (_, ret1, _)) ->
                        Ok (te, Nothing, (funcType, ret || ret1, loops))
              else
                Bad ("at " ++ (lineInfoString lineInfo) ++ " Expression has type " ++ (printTree ft) ++ ". Expected Array or List")
    ForRange lineInfo ident expr1 expr2 stmt ->
      case typecheckExpr expr1 te scope of
        Bad str ->
          Bad str
        Ok (_, res, _) ->
          case res of
            Nothing ->
              Bad ("at " ++ (lineInfoString lineInfo) ++ " Expression has no type. Expected Int")
            Just ft ->
              if isInt ft then
                case typecheckExpr expr2 te scope of
                  Bad str ->
                    Bad str
                  Ok (_, res, _) ->
                    case res of
                      Nothing ->
                        Bad ("at " ++ (lineInfoString lineInfo) ++ " Expression has no type. Expected Int")
                      Just ft ->
                        if isInt ft then
                          case typecheckStmt stmt (extendFunc te ident (Just (FullType lineInfo [TModReadonly lineInfo] (Int lineInfo)))) (funcType, ret, loops + 1) of
                            Bad str ->
                              Bad str
                            Ok (_, _, (_, ret1, _)) ->
                              Ok (te, Nothing, (funcType, ret || ret1, loops))
                        else
                          Bad ("at " ++ (lineInfoString lineInfo) ++ " Expression has type " ++ (printTree ft) ++ ". Expected Int")
              else
                Bad ("at " ++ (lineInfoString lineInfo) ++ " Expression has type " ++ (printTree ft) ++ ". Expected Int")
    Composition _ stmt1 stmt2 ->
      case typecheckStmt stmt1 te scope of
        Bad str ->
          Bad str
        Ok (te2, _, scope2) ->
          case typecheckStmt stmt2 te2 scope2 of
            Bad str ->
              Bad str
            Ok (te3, res, scope3) ->
              Ok (te3, res, scope3)
typecheckType :: Type (Maybe (Int, Int)) -> TEnv -> Scope -> TypeCheckResult
typecheckType x te scope =
  case x of
    Int _ ->
      Ok (te, Nothing, scope)
    Str _ ->
      Ok (te, Nothing, scope)
    Bool _ ->
      Ok (te, Nothing, scope)
    Void _ ->
      Ok (te, Nothing, scope)
    Float _ ->
      Ok (te, Nothing, scope)
    List _ fulltype ->
      case typecheckFullType fulltype te scope of
        Bad str ->
          Bad str
        Ok (_, _, _) ->
          Ok (te, Nothing, scope)
    Array _ fulltype ->
      case typecheckFullType fulltype te scope of
        Bad str ->
          Bad str
        Ok (_, _, _) ->
          Ok (te, Nothing, scope)
    Fun _ argtypes fulltype ->
      let
        go [] =
          Ok (te, Nothing, scope)
        go (argtype:args) =
          case typecheckArgType argtype te scope of
            Bad str ->
              Bad str
            Ok (_, _, _) ->
              go args
      in
        case go argtypes of
          Bad str ->
            Bad str
          Ok (_, _, _) ->
            case typecheckFullType fulltype te scope of
              Bad str ->
                Bad str
              Ok (_, _, _) ->
                Ok (te, Nothing, scope)
typecheckArgType :: ArgType (Maybe (Int, Int)) -> TEnv -> Scope -> TypeCheckResult
typecheckArgType x te scope =
  case x of
    ArgType _ argmod type_ ->
      case typecheckArgMod argmod te scope of
        Bad str ->
          Bad str
        Ok (_, _, _) ->
          Ok (te, Nothing, scope)
typecheckFullType :: FullType (Maybe (Int, Int)) -> TEnv -> Scope -> TypeCheckResult
typecheckFullType x te scope =
  case x of
    FullType _ typemods type_ ->
      let
        go [] =
          Ok (te, Nothing, scope)
        go (tm:tms) =
          case typecheckTypeMod tm te scope of
            Bad str ->
              Bad str
            Ok (_, _, _) ->
              go tms
      in
        case go typemods of
          Bad str ->
            Bad str
          Ok (_, _, _) ->
            case typecheckType type_ te scope of
              Bad str ->
                Bad str
              Ok (_, _, _) ->
                Ok (te, Nothing, scope)
typecheckArgMod :: ArgMod (Maybe (Int, Int)) -> TEnv -> Scope -> TypeCheckResult
typecheckArgMod x te scope =
  case x of
    AModVar _ ->
      Ok (te, Nothing, scope)
    AModVal _ ->
      Ok (te, Nothing, scope)
    AModInOut _ ->
      Ok (te, Nothing, scope)
typecheckTypeMod :: TypeMod (Maybe (Int, Int)) -> TEnv -> Scope -> TypeCheckResult
typecheckTypeMod x te scope =
  case x of
    TModReadonly _ ->
      Ok (te, Nothing, scope)
typecheckExpr :: Expr (Maybe (Int, Int)) -> TEnv -> Scope -> TypeCheckResult
typecheckExpr x te scope =
  case x of
    EVar lineInfo ident ->
      case te ident of
        Just res ->
          Ok (te, Just res, scope)
        Nothing ->
          Bad ("at " ++ (lineInfoString lineInfo) ++ " Undefined variable " ++ (printTree ident))
    ELitInt lineInfo integer ->
      Ok (te, Just (FullType lineInfo [TModReadonly lineInfo] (Int lineInfo)), scope)
    ELitTrue lineInfo ->
      Ok (te, Just (FullType lineInfo [TModReadonly lineInfo] (Bool lineInfo)), scope)
    ELitFalse lineInfo ->
      Ok (te, Just (FullType lineInfo [TModReadonly lineInfo] (Bool lineInfo)), scope)
    EString lineInfo string ->
      Ok (te, Just (FullType lineInfo [TModReadonly lineInfo] (Str lineInfo)), scope)
    ELitFloat lineInfo double ->
      Ok (te, Just (FullType lineInfo [TModReadonly lineInfo] (Float lineInfo)), scope)
    EEmpList lineInfo fulltype ->
      case typecheckFullType fulltype te scope of
        Bad str ->
          Bad str
        Ok (_, _, _) ->
          Ok (te, Just (FullType lineInfo [TModReadonly lineInfo] (List lineInfo fulltype)), scope)
    Neg lineInfo expr ->
      case typecheckExpr expr te scope of
        Bad str ->
          Bad str
        Ok (_, res, _) ->
          case res of
            Nothing ->
              Bad ("at " ++ (lineInfoString lineInfo) ++ " Expected expression type Int")
            Just ft ->
              if isInt ft then
                Ok (te, res, scope)
              else
                Bad ("at " ++ (lineInfoString lineInfo) ++ " Expected expression type Int. Got " ++ (printTree ft))
    Not lineInfo expr ->
      case typecheckExpr expr te scope of
        Bad str ->
          Bad str
        Ok (_, res, _) ->
          case res of
            Nothing ->
              Bad ("at " ++ (lineInfoString lineInfo) ++ " Expected expression type Bool")
            Just ft ->
              if isBool ft then
                Ok (te, res, scope)
              else
                Bad ("at " ++ (lineInfoString lineInfo) ++ " Expected expression type Bool. Got " ++ (printTree ft))
    ECons lineInfo expr1 expr2 ->
      case typecheckExpr expr2 te scope of
        Bad str ->
          Bad str
        Ok (_, res, _) ->
          case res of
            Nothing ->
              Bad ("at " ++ (lineInfoString lineInfo) ++ " Expected expression type List")
            Just ft ->
              if isList ft then
                let
                  xelmft = listElementType ft
                in
                  case typecheckExpr expr1 te scope of
                    Bad str ->
                      Bad str
                    Ok (_, elres, _) ->
                      case (elres, xelmft) of
                        (Nothing, _) ->
                          Bad ("at " ++ (lineInfoString lineInfo) ++ " Expression with no type")
                        (_, Nothing) ->
                          Bad ("at " ++ (lineInfoString lineInfo) ++ " Expression with no type")
                        (Just elft, Just xelft) ->
                          if matchFullType xelft elft then
                            Ok (te, Just ft, scope)
                          else
                            Bad ("at " ++ (lineInfoString lineInfo) ++ " Expected expression type " ++ (printTree xelft) ++ ". Got " ++ (printTree elft))
              else
                Bad ("at " ++ (lineInfoString lineInfo) ++ " Expected expression type List. Got " ++ (printTree ft))
    EMul lineInfo expr1 mulop expr2 ->
      case (typecheckExpr expr1 te scope, typecheckExpr expr2 te scope) of
        (Bad str, _) ->
          Bad str
        (_, Bad str) ->
          Bad str
        (Ok (_, res1, _), Ok (_, res2, _)) ->
          case (res1, res2) of
            (Nothing, _) ->
              Bad ("at " ++ (lineInfoString lineInfo) ++ " Multiplying expression without type")
            (_, Nothing) ->
              Bad ("at " ++ (lineInfoString lineInfo) ++ " Multiplying expression without type")
            (Just ft1, Just ft2) ->
              if (isInt ft1 && isInt ft2) || (isFloat ft1 && isFloat ft2) then
                Ok (te, res1, scope)
              else
                Bad ("at " ++ (lineInfoString lineInfo) ++ " Multiplying expressions of different or non number types")
    EAdd lineInfo expr1 addop expr2 ->
      case (typecheckExpr expr1 te scope, typecheckExpr expr2 te scope) of
        (Bad str, _) ->
          Bad str
        (_, Bad str) ->
          Bad str
        (Ok (_, res1, _), Ok (_, res2, _)) ->
          case (res1, res2) of
            (Nothing, _) ->
              Bad ("at " ++ (lineInfoString lineInfo) ++ " Adding expression without type")
            (_, Nothing) ->
              Bad ("at " ++ (lineInfoString lineInfo) ++ " Adding expression without type")
            (Just ft1, Just ft2) ->
              if (isInt ft1 && isInt ft2) || (isFloat ft1 && isFloat ft2) then
                Ok (te, res1, scope)
              else
                Bad ("at " ++ (lineInfoString lineInfo) ++ " Adding expressions of different or non number types")
    ERel lineInfo expr1 relop expr2 ->
      case typecheckExpr expr1 te scope of
        Bad str -> Bad str
        Ok (_, mft1, _) ->
          case typecheckExpr expr2 te scope of
            Bad str -> Bad str
            Ok (_, mft2, _) ->
              case (mft1, mft2) of
                (Just ft1, Just ft2) -> 
                  if (matchFullType ft1 ft2) then
                    if isInt ft1 || isFloat ft1 then
                      Ok (te, Just (FullType lineInfo [TModReadonly lineInfo] (Bool lineInfo)), scope)
                    else
                      Bad ("at " ++ (lineInfoString lineInfo) ++ " Cannot compare objects of type " ++ (printTree ft1))
                  else
                    Bad ("at " ++ (lineInfoString lineInfo) ++ " Cannot compare " ++ (printTree ft1) ++ " with " ++ (printTree ft2))
                otherwise -> Bad ("at " ++ (lineInfoString lineInfo) ++ " Compared expression has no type")
    EAnd lineInfo expr1 andop expr2 ->
      case (typecheckExpr expr1 te scope, typecheckExpr expr2 te scope) of
        (Bad str, _) ->
          Bad str
        (_, Bad str) ->
          Bad str
        (Ok (_, res1, _), Ok (_, res2, _)) ->
          case (res1, res2) of
            (Nothing, _) ->
              Bad ("at " ++ (lineInfoString lineInfo) ++ " Boolean operator applied to expression of wrong type")
            (_, Nothing) ->
              Bad ("at " ++ (lineInfoString lineInfo) ++ " Boolean operator applied to expression of wrong type")
            (Just ft1, Just ft2) ->
              if isBool ft1 && isBool ft2 then
                Ok (te, res1, scope)
              else
                Bad ("at " ++ (lineInfoString lineInfo) ++ " Boolean operator applied to expression of wrong type")
    EOr lineInfo expr1 orop expr2 ->
      case (typecheckExpr expr1 te scope, typecheckExpr expr2 te scope) of
        (Bad str, _) ->
          Bad str
        (_, Bad str) ->
          Bad str
        (Ok (_, res1, _), Ok (_, res2, _)) ->
          case (res1, res2) of
            (Nothing, _) ->
              Bad ("at " ++ (lineInfoString lineInfo) ++ " Boolean operator applied to expression of wrong type")
            (_, Nothing) ->
              Bad ("at " ++ (lineInfoString lineInfo) ++ " Boolean operator applied to expression of wrong type")
            (Just ft1, Just ft2) ->
              if isBool ft1 && isBool ft2 then
                Ok (te, res1, scope)
              else
                Bad ("at " ++ (lineInfoString lineInfo) ++ " Boolean operator applied to expression of wrong type")
    EList lineInfo exprs ->
      let
        go (e:nil) =
          case typecheckExpr e te scope of
            Bad str ->
              Bad str
            Ok (_, res, _) ->
              Ok (te, res, scope)
        go (e:es) =
          case typecheckExpr e te scope of
            Bad str ->
              Bad str
            Ok (_, res, _) ->
              case go es of
                Bad str ->
                  Bad str
                Ok (_, res2, _) ->
                  case (res, res2) of
                    (Nothing, _) ->
                      Bad ("at " ++ (lineInfoString lineInfo) ++ " Expression without type inside a List")
                    (_, Nothing) ->
                      Bad ("at " ++ (lineInfoString lineInfo) ++ " Expression without type inside a List")
                    (Just ft1, Just ft2) ->
                      if matchFullType ft1 ft2 then
                        Ok (te, Just ft1, scope)
                      else
                        Bad ("at " ++ (lineInfoString lineInfo) ++ " List has two different element types (" ++ (printTree ft1) ++ " and " ++ (printTree ft2) ++ ")")
      in
        case go exprs of
          Bad str ->
            Bad str
          Ok (_, res, _) ->
            case res of
              Nothing ->
                Bad ("at " ++ (lineInfoString lineInfo) ++ " List elements have to have types")
              Just ft ->
                Ok (te, Just (FullType lineInfo [TModReadonly lineInfo] (List lineInfo ft)), scope)
    EArr lineInfo exprs ->
      let
        go (e:nil) =
          case typecheckExpr e te scope of
            Bad str ->
              Bad str
            Ok (_, res, _) ->
              Ok (te, res, scope)
        go (e:es) =
          case typecheckExpr e te scope of
            Bad str ->
              Bad str
            Ok (_, res, _) ->
              case go es of
                Bad str ->
                  Bad str
                Ok (_, res2, _) ->
                  case (res, res2) of
                    (Nothing, _) ->
                      Bad ("at " ++ (lineInfoString lineInfo) ++ " Expression without type inside an Array")
                    (_, Nothing) ->
                      Bad ("at " ++ (lineInfoString lineInfo) ++ " Expression without type inside an Array")
                    (Just ft1, Just ft2) ->
                      if matchFullType ft1 ft2 then
                        Ok (te, Just ft1, scope)
                      else
                        Bad ("at " ++ (lineInfoString lineInfo) ++ " Array has two different element types (" ++ (printTree ft1) ++ " and " ++ (printTree ft2) ++ ")")
      in
        case go exprs of
          Bad str ->
            Bad str
          Ok (_, res, _) ->
            case res of
              Nothing ->
                Bad ("at " ++ (lineInfoString lineInfo) ++ " Array elements have to have types")
              Just ft ->
                Ok (te, Just (FullType lineInfo [TModReadonly lineInfo] (Array lineInfo ft)), scope)
    EArrSize lineInfo fulltype expr ->
      case typecheckExpr expr te scope of
        Bad str ->
          Bad str
        Ok (_, res, _) ->
          case res of
            Nothing ->
              Bad ("at " ++ (lineInfoString lineInfo) ++ " Expected expression type Int")
            Just ft ->
              if isInt ft then
                Ok (te, Just (FullType lineInfo [TModReadonly lineInfo] (Array lineInfo fulltype)), scope)
              else
                Bad ("at " ++ (lineInfoString lineInfo) ++ " Expected expression type Int")
    EApp lineInfo expr exprs ->
      case typecheckExpr expr te scope of
        Bad str ->
          Bad str
        Ok (_, mft, _) ->
          case mft of
            Nothing ->
              Bad ("at " ++ (lineInfoString lineInfo) ++ " Expected expression type Function")
            Just fun ->
              case fun of
                FullType _ _ (Fun _ ats ft) ->
                  let
                    go [] [] =
                      Ok (te, Nothing, scope)
                    go [] e =
                      Bad ("at " ++ (lineInfoString lineInfo) ++ " Too many arguments")
                    go a [] =
                      Bad ("at " ++ (lineInfoString lineInfo) ++ " Too few arguments")
                    go (a:as) (e:es) =
                      case typecheckExpr e te scope of
                        Bad str ->
                          Bad str
                        Ok (_, res, _) ->
                          case res of
                            Nothing ->
                              Bad ("at " ++ (lineInfoString lineInfo) ++ " Argument expression has no type")
                            Just f ->
                              case a of
                                ArgType _ _ ft1 ->
                                  if matchFullType ft1 f then
                                    go as es
                                  else
                                    Bad ("at " ++ (lineInfoString lineInfo) ++ " Wrong argument type. Expected " ++ (printTree ft1) ++ ", got " ++ (printTree f))
                                otherwise ->
                                  Bad ("at " ++ (lineInfoString lineInfo) ++ " Wrong argument type")
                  in
                    case go ats exprs of
                      Bad str ->
                        Bad str
                      Ok (_, _, _) ->
                        Ok (te, Just ft, scope)
                otherwise ->
                  Bad ("at " ++ (lineInfoString lineInfo) ++ " Expected expression type Function")
    EArrApp lineInfo expr1 expr2 ->
      case typecheckExpr expr1 te scope of
        Bad str ->
          Bad str
        Ok (_, res, _) ->
          case res of
            Nothing ->
              Bad ("at " ++ (lineInfoString lineInfo) ++ " Expected expression type Array")
            Just ft1 ->
              if isArray ft1 then
                case typecheckExpr expr2 te scope of
                  Bad str ->
                    Bad str
                  Ok (_, res2, _) ->
                    case res2 of
                      Nothing ->
                        Bad ("at " ++ (lineInfoString lineInfo) ++ " Expected expression type Int")
                      Just ft2 ->
                        if isInt ft2 then
                          Ok (te, arrayElementType ft1, scope)
                        else
                          Bad ("at " ++ (lineInfoString lineInfo) ++ " Expected expression type Int")
              else
                Bad ("at " ++ (lineInfoString lineInfo) ++ " Expected expression type Array")
    EIf lineInfo expr1 expr2 expr3 ->
      case typecheckExpr expr1 te scope of
        Bad str ->
          Bad str
        Ok (_, res1, _) ->
          case res1 of
            Nothing ->
              Bad ("at " ++ (lineInfoString lineInfo) ++ " Expected expression type Bool")
            Just ft1 ->
              if isBool ft1 then
                case (typecheckExpr expr2 te scope, typecheckExpr expr3 te scope) of
                  (Bad str, _) ->
                    Bad str
                  (_, Bad str) ->
                    Bad str
                  (Ok (_, res2, _), Ok (_, res3, _)) ->
                    case (res2, res3) of
                      (Nothing, _) ->
                        Bad ("at " ++ (lineInfoString lineInfo) ++ " Expression inside if has no type")
                      (_, Nothing) ->
                        Bad ("at " ++ (lineInfoString lineInfo) ++ " Expression inside if has no type")
                      (Just ft2, Just ft3) ->
                        if matchFullType ft2 ft3 then
                          Ok (te, Just ft2, scope)
                        else
                          Bad ("at " ++ (lineInfoString lineInfo) ++ " If branches have different types: '" ++ (printTree ft2) ++ "' and '" ++ (printTree ft3) ++ "'")
              else
                Bad ("at " ++ (lineInfoString lineInfo) ++ " Expected expression type Bool")
    ELambda lineInfo fulltype args stmt ->
      case typecheckStmt stmt (extendFuncArgs te args) (Just fulltype, False, 0) of
        Bad str ->
          Bad str
        Ok (_, _, (_, ret, _)) ->
          if ret then
            Ok (te, Just (FullType lineInfo [] (Fun lineInfo (argsToArgTypes args) fulltype)), scope)
          else
            Bad ("at " ++ (lineInfoString lineInfo) ++ " Function is missing return statement")
    ERand lineInfo expr ->
      case typecheckExpr expr te scope of
        Bad str ->
          Bad str
        Ok (_, Nothing, _) ->
          Bad ("at " ++ (lineInfoString lineInfo) ++ " Expected expression type List or Array")
        Ok (_, Just ft, _) ->
          case elementType ft of
            Nothing ->
              Bad ("at " ++ (lineInfoString lineInfo) ++ " Expected expression type List or Array")
            Just elft ->
              Ok (te, Just elft, scope)
    ERandDist lineInfo expr1 expr2 ->
      case typecheckExpr expr1 te scope of
        Bad str ->
          Bad str
        Ok (_, Nothing, _) ->
          Bad ("at " ++ (lineInfoString lineInfo) ++ " Expected expression type List or Array")
        Ok (_, Just ft1, _) ->
          if isList ft1 || isArray ft1 then
            case typecheckExpr expr2 te scope of
              Bad str ->
                Bad str
              Ok (_, Nothing, _) ->
                Bad ("at " ++ (lineInfoString lineInfo) ++ " Expected expression type List or Array of Float")
              Ok (_, Just ft2, _) ->
                if isList ft2 then
                  case listElementType ft2 of
                    Nothing ->
                      Bad ("at " ++ (lineInfoString lineInfo) ++ " Expected expression type List or Array of Float")
                    Just elft ->
                      if matchFullType elft (FullType lineInfo [] (Float lineInfo)) then
                        Ok (te, elementType ft1, scope)
                      else
                        Bad ("at " ++ (lineInfoString lineInfo) ++ " Expected expression type List or Array of Float")
                else if isArray ft2 then
                  case arrayElementType ft2 of
                    Nothing ->
                      Bad ("at " ++ (lineInfoString lineInfo) ++ " Expected expression type List or Array of Float")
                    Just elft ->
                      if matchFullType elft (FullType lineInfo [] (Float lineInfo)) then
                        Ok (te, Just (FullType lineInfo [TModReadonly lineInfo] (Float lineInfo)), scope)
                      else
                        Bad ("at " ++ (lineInfoString lineInfo) ++ " Expected expression type List or Array of Float")
                else
                  Bad ("at " ++ (lineInfoString lineInfo) ++ " Expected expression type List or Array of Float")
          else
            Bad ("at " ++ (lineInfoString lineInfo) ++ " Expected expression type List or Array")
    EProb lineInfo stmt expr ->
      case typecheckStmt stmt te (Nothing, False, 0) of
        Bad str ->
          Bad str
        Ok (te2, _, _) ->
          case typecheckExpr expr te2 scope of
            Bad str ->
              Bad str
            Ok (_, Nothing, _) ->
              Bad ("at " ++ (lineInfoString lineInfo) ++ " Expected expression type Bool")
            Ok (_, Just ft2, _) ->
              if isBool ft2 then
                Ok (te, Just (FullType lineInfo [TModReadonly lineInfo] (Float lineInfo)), scope)
              else
                Bad ("at " ++ (lineInfoString lineInfo) ++ " Expected expression type Bool")
    EProbSamp lineInfo expr1 stmt expr2 ->
      case typecheckExpr expr1 te scope of
        Bad str ->
          Bad str
        Ok (_, Nothing, _) ->
          Bad ("at " ++ (lineInfoString lineInfo) ++ " Expected expression type Int")
        Ok (_, Just ft1, _) ->
          if isInt ft1 then
            case typecheckStmt stmt te (Nothing, False, 0) of
              Bad str ->
                Bad str
              Ok (te2, _, _) ->
                case typecheckExpr expr2 te2 scope of
                  Bad str ->
                    Bad str
                  Ok (_, Nothing, _) ->
                    Bad ("at " ++ (lineInfoString lineInfo) ++ " Expected expression type Bool")
                  Ok (_, Just ft2, _) ->
                    if isBool ft2 then
                      Ok (te, Just (FullType lineInfo [TModReadonly lineInfo] (Float lineInfo)), scope)
                    else
                      Bad ("at " ++ (lineInfoString lineInfo) ++ " Expected expression type Bool")
          else
            Bad ("at " ++ (lineInfoString lineInfo) ++ " Expected expression type Int")
typecheckAddOp :: AddOp (Maybe (Int, Int)) -> TEnv -> Scope -> TypeCheckResult
typecheckAddOp x te scope =
  case x of
    Plus _ ->
      Ok (te, Nothing, scope)
    Minus _ ->
      Ok (te, Nothing, scope)
typecheckMulOp :: MulOp (Maybe (Int, Int)) -> TEnv -> Scope -> TypeCheckResult
typecheckMulOp x te scope =
  case x of
    Times _ ->
      Ok (te, Nothing, scope)
    Div _ ->
      Ok (te, Nothing, scope)
    Mod _ ->
      Ok (te, Nothing, scope)
typecheckRelOp :: RelOp (Maybe (Int, Int)) -> TEnv -> Scope -> TypeCheckResult
typecheckRelOp x te scope =
  case x of
    LTH _ ->
      Ok (te, Nothing, scope)
    LE _ ->
      Ok (te, Nothing, scope)
    GTH _ ->
      Ok (te, Nothing, scope)
    GE _ ->
      Ok (te, Nothing, scope)
    EQU _ ->
      Ok (te, Nothing, scope)
    NE _ ->
      Ok (te, Nothing, scope)
typecheckOrOp :: OrOp (Maybe (Int, Int)) -> TEnv -> Scope -> TypeCheckResult
typecheckOrOp x te scope =
  case x of
    Or _ ->
      Ok (te, Nothing, scope)
typecheckAndOp :: AndOp (Maybe (Int, Int)) -> TEnv -> Scope -> TypeCheckResult
typecheckAndOp x te scope =
  case x of
    And _ ->
      Ok (te, Nothing, scope)