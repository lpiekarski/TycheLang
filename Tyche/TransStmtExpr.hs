module Tyche.TransStmtExpr where

import           Tyche.Abs
import           Tyche.Bool
import           Tyche.Converters
import           Tyche.Env
import           Tyche.Numerical
import           Tyche.Print
import           Tyche.State
import           Tyche.Trans
import           Tyche.Types

import           Data.Array

funcApp :: Expr LineInfo -> [Expr LineInfo] -> VEnv -> LEnv -> ECont -> Cont
funcApp expr exprs venv lenv econt =
  transExpr expr venv lenv (\val ->
    case val of
      FuncVal args func ->
        let
          exprsToArgVals :: [Expr LineInfo] -> [Arg LineInfo] -> [ArgVal] -> ([ArgVal] -> Cont) -> Cont
          exprsToArgVals [] [] acc avcont = avcont (reverse acc)
          exprsToArgVals (e:es) ((Arg _ argmod argident argfulltype):as) acc avcont =
            transExpr e venv lenv (\val ->
              let
                av = case argmod of
                  AModVar _ ->
                    case e of
                      EVar _ ident ->
                        case venv ident of
                          Just loc -> Variable loc argident
                          Nothing  -> ArgError
                      otherwise -> ArgError
                  AModVal _ -> Value val argident
                  AModInOut _ ->
                    case e of
                      EVar _ ident ->
                        case venv ident of
                            Just loc -> Inout val loc argident
                            Nothing  -> ArgError
                      otherwise -> ArgError
              in
                exprsToArgVals es as (av:acc) avcont
            )
        in
          exprsToArgVals exprs args [] (\argvals -> do
            let
              addArgValsToVEnv :: [ArgVal] -> VEnv -> [Maybe Loc] -> (VEnv -> [Maybe Loc] -> Cont) -> Cont
              addArgValsToVEnv [] acc locs venvloccont = venvloccont acc (reverse locs)
              addArgValsToVEnv (av:avs) acc locs venvloccont =
                case av of
                  Variable loc argident -> addArgValsToVEnv avs (extendFunc acc argident (Just loc)) (Nothing:locs) venvloccont
                  Value val argident -> \(store, input) -> do
                    let (varloc, storeaftervardef) = newLoc store
                    let venvaftervardef = extendFunc acc argident (Just varloc)
                    let storeaftervalsave = saveInStore storeaftervardef varloc val
                    addArgValsToVEnv avs venvaftervardef (Nothing:locs) venvloccont (storeaftervalsave, input)
                  Inout val loc argident -> \(store, input) -> do
                    let (varloc, storeaftervardef) = newLoc store
                    let venvaftervardef = extendFunc acc argident (Just varloc)
                    let storeaftervalsave = saveInStore storeaftervardef varloc val
                    addArgValsToVEnv avs venvaftervardef ((Just varloc):locs) venvloccont (storeaftervalsave, input)
                  ArgError -> errMsg "Argument error\n"
            let
              venvloccont = (\venvwithargs -> \inoutlocs ->
                let
                  restoreInouts :: [ArgVal] -> [Maybe Loc] -> Store -> Cont -> Cont
                  restoreInouts [] [] afterfuncstore cont = cont
                  restoreInouts (av:avs) (l:ls) (afterfuncstore@(next, storefunc)) cont = case av of
                    Variable _ _ -> restoreInouts avs ls afterfuncstore cont
                    Value _ _ -> restoreInouts avs ls afterfuncstore cont
                    Inout _ loc argident -> \(store, input) -> case l of
                      Nothing -> errMsg "Inout parameter without location\n" (store, input)
                      Just ploc -> do
                        let storeafter = saveInStore store loc (storefunc ploc)
                        restoreInouts avs ls afterfuncstore cont (storeafter, input)
                  econtinouted val (afterfuncstore, afterfuncinput) =
                    restoreInouts argvals inoutlocs afterfuncstore (econt val) (afterfuncstore, afterfuncinput)
                in
                  func argvals venvwithargs (addReturnLabel lenv econtinouted) (\v -> econtinouted NoVal))
            addArgValsToVEnv argvals (\x -> Nothing) [] venvloccont)
      otherwise    -> errMsg "Expected a function\n")

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
    let funcval = FuncVal args (\funcargs -> \callvenv -> transStmts stmts (mergeVEnv venvafterfuncvardef callvenv))
    let storeafterfuncvarsave = saveInStore storeafterfuncvardef funcvarloc funcval
    let cont = icont venvafterfuncvardef
    cont (storeafterfuncvarsave, input)
  FnApp _ expr exprs -> funcApp expr exprs venv lenv (\v -> icont venv)
  Cond _ expr stmts ->
    transExpr expr venv lenv (\val ->
      case val of
        BoolVal boolval ->
          if boolval then transStmts stmts venv lenv (\v -> icont venv)
          else icont venv
        otherwise -> errMsg "Value inside If expression is not bool\n")
  CondElse _ expr stmts1 stmts2 ->
    transExpr expr venv lenv (\val ->
      case val of
        BoolVal boolval ->
          if boolval then transStmts stmts1 venv lenv (\v -> icont venv)
          else transStmts stmts2 venv lenv (\v -> icont venv)
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

transExpr :: Expr LineInfo -> VEnv -> LEnv -> ECont -> Cont
transExpr x venv lenv econt = case x of
  ELitVoid _ -> econt NoVal
  EVar _ ident -> case venv ident of
    Nothing -> errMsg ("Undefined variable `" ++ (printTree ident) ++ "`\n")
    Just loc -> \(store@(next, storef), input) -> do
      let cont = econt (storef loc)
      cont (store, input)
  ELitInt _ integer -> econt (IntVal integer)
  ELitTrue _ -> econt (BoolVal True)
  ELitFalse _ -> econt (BoolVal False)
  EString _ string -> do
    let
      firstLast xs@(_:_) = tail (init xs)
      firstLast _        = []
    econt (StringVal (firstLast string))
  ELitFloat _ double -> econt (FloatVal double)
  EEmpList _ fulltype -> econt (ListVal [])
  EEmpArray _ fulltype -> econt (ArrayVal (listArray (0, 0) []))
  EApp _ expr exprs -> funcApp expr exprs venv lenv econt
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
  EArrSize _ expr1 expr2 fulltype ->
    transExpr expr1 venv lenv (\val1 ->
      case val1 of
        IntVal intval ->
          transExpr expr2 venv lenv (\val2 ->
            econt (ArrayVal (listArray (0, (fromIntegral intval) - 1) (replicate (fromIntegral intval) val2))))
        otherwise -> errMsg "Expected int value\n")
  EArrApp _ expr1 expr2 ->
    transExpr expr1 venv lenv (\val1 ->
      case val1 of
        ArrayVal a ->
          transExpr expr2 venv lenv (\val2 ->
            case val2 of
              IntVal intval2 -> econt (a ! (fromIntegral intval2))
              otherwise      -> errMsg "Expected int value\n")
        otherwise -> errMsg "Expected array value\n")
  EIf _ expr1 expr2 expr3 ->
    transExpr expr1 venv lenv (\val1 ->
      case val1 of
        BoolVal bval ->
          if bval then
            transExpr expr2 venv lenv econt
          else
            transExpr expr3 venv lenv econt
        otherwise -> errMsg "Expected bool value\n")
  ELambda _ fulltype args stmts -> do
    let funcval = FuncVal args (\funcargs -> \callvenv -> transStmts stmts (mergeVEnv venv callvenv))
    econt funcval
  ERand _ expr ->
    transExpr expr venv lenv (\val -> \(store, (istream, (randint:randstream))) ->
      let
        list = case val of
          ListVal l  -> l
          ArrayVal a -> elems a
      in
        econt (list!!(randint `mod` (length list))) (store, (istream, randstream)))
  EProbSamp _ expr1 stmts expr2 ->
    transExpr expr1 venv lenv (\samplesval -> \(store, input) -> case samplesval of
      IntVal samples ->
        let
          iterProb :: Int -> Int -> (Int -> Cont) -> Cont
          iterProb 0 acc intcont = intcont acc
          iterProb i acc intcont =
            transStmts stmts venv lenv (\aftervenv ->
              transExpr expr2 aftervenv lenv (\val -> \(afterstore, afterinput) -> case val of
                BoolVal bval ->
                  if bval then
                    iterProb (i - 1) (acc + 1) intcont (store, afterinput)
                  else
                    iterProb (i - 1) acc intcont (store, afterinput)
                otherwise -> errMsg "Probability check expression should have type bool\n" (afterstore, afterinput))
              )
        in
          iterProb (fromIntegral samples) 0 (\satisfied -> econt (FloatVal ((fromIntegral satisfied) / (fromIntegral samples)))) (store, input)
      otherwise -> errMsg "Number of samples must be an integer\n" (store, input))
