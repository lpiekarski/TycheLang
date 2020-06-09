-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Tyche.Par where
import Tyche.Abs
import Tyche.Lex
import Tyche.ErrM

}

-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype {Token}
%name pProgram_internal Program
%name pArg_internal Arg
%name pListArg_internal ListArg
%name pStmt_internal Stmt
%name pListStmt_internal ListStmt
%name pType_internal Type
%name pArgType_internal ArgType
%name pFullType_internal FullType
%name pListArgType_internal ListArgType
%name pArgMod_internal ArgMod
%name pTypeMod_internal TypeMod
%name pListTypeMod_internal ListTypeMod
%name pExpr9_internal Expr9
%name pExpr8_internal Expr8
%name pExpr7_internal Expr7
%name pExpr6_internal Expr6
%name pExpr5_internal Expr5
%name pExpr4_internal Expr4
%name pExpr3_internal Expr3
%name pExpr2_internal Expr2
%name pExpr1_internal Expr1
%name pExpr_internal Expr
%name pListExpr_internal ListExpr
%name pAddOp_internal AddOp
%name pMulOp_internal MulOp
%name pRelOp_internal RelOp
%name pOrOp_internal OrOp
%name pAndOp_internal AndOp
%token
  '!' { PT _ (TS _ 1) }
  '!=' { PT _ (TS _ 2) }
  '%' { PT _ (TS _ 3) }
  '&&' { PT _ (TS _ 4) }
  '(' { PT _ (TS _ 5) }
  ')' { PT _ (TS _ 6) }
  '*' { PT _ (TS _ 7) }
  '+' { PT _ (TS _ 8) }
  ',' { PT _ (TS _ 9) }
  '-' { PT _ (TS _ 10) }
  '->' { PT _ (TS _ 11) }
  '.' { PT _ (TS _ 12) }
  '/' { PT _ (TS _ 13) }
  ':' { PT _ (TS _ 14) }
  ';' { PT _ (TS _ 15) }
  '<' { PT _ (TS _ 16) }
  '<=' { PT _ (TS _ 17) }
  '=' { PT _ (TS _ 18) }
  '==' { PT _ (TS _ 19) }
  '>' { PT _ (TS _ 20) }
  '>=' { PT _ (TS _ 21) }
  '?' { PT _ (TS _ 22) }
  '[' { PT _ (TS _ 23) }
  '[]' { PT _ (TS _ 24) }
  ']' { PT _ (TS _ 25) }
  'and' { PT _ (TS _ 26) }
  'array' { PT _ (TS _ 27) }
  'boolean' { PT _ (TS _ 28) }
  'break' { PT _ (TS _ 29) }
  'continue' { PT _ (TS _ 30) }
  'def' { PT _ (TS _ 31) }
  'do' { PT _ (TS _ 32) }
  'each' { PT _ (TS _ 33) }
  'else' { PT _ (TS _ 34) }
  'equals' { PT _ (TS _ 35) }
  'false' { PT _ (TS _ 36) }
  'float' { PT _ (TS _ 37) }
  'for' { PT _ (TS _ 38) }
  'from' { PT _ (TS _ 39) }
  'if' { PT _ (TS _ 40) }
  'inout' { PT _ (TS _ 41) }
  'int' { PT _ (TS _ 42) }
  'lambda' { PT _ (TS _ 43) }
  'mod' { PT _ (TS _ 44) }
  'not' { PT _ (TS _ 45) }
  'of' { PT _ (TS _ 46) }
  'or' { PT _ (TS _ 47) }
  'probability' { PT _ (TS _ 48) }
  'random' { PT _ (TS _ 49) }
  'readonly' { PT _ (TS _ 50) }
  'return' { PT _ (TS _ 51) }
  'sampled' { PT _ (TS _ 52) }
  'satisfying' { PT _ (TS _ 53) }
  'skip' { PT _ (TS _ 54) }
  'string' { PT _ (TS _ 55) }
  'times' { PT _ (TS _ 56) }
  'to' { PT _ (TS _ 57) }
  'true' { PT _ (TS _ 58) }
  'val' { PT _ (TS _ 59) }
  'var' { PT _ (TS _ 60) }
  'void' { PT _ (TS _ 61) }
  'while' { PT _ (TS _ 62) }
  '{' { PT _ (TS _ 63) }
  '{}' { PT _ (TS _ 64) }
  '||' { PT _ (TS _ 65) }
  '}' { PT _ (TS _ 66) }

  L_ident {PT _ (TV _)}
  L_integ {PT _ (TI _)}
  L_quoted {PT _ (TL _)}
  L_doubl {PT _ (TD _)}

%%

Ident :: {
  (Maybe (Int, Int), Ident)
}
: L_ident {
  (Just (tokenLineCol $1), Ident (prToken $1)) 
}

Integer :: {
  (Maybe (Int, Int), Integer)
}
: L_integ {
  (Just (tokenLineCol $1), read (prToken $1)) 
}

String :: {
  (Maybe (Int, Int), String)
}
: L_quoted {
  (Just (tokenLineCol $1), prToken $1)
}

Double :: {
  (Maybe (Int, Int), Double)
}
: L_doubl {
  (Just (tokenLineCol $1), read (prToken $1)) 
}

Program :: {
  (Maybe (Int, Int), Program (Maybe (Int, Int)))
}
: 'do' '{' ListStmt '}' {
  (Just (tokenLineCol $1), Tyche.Abs.Program (Just (tokenLineCol $1)) (snd $3)) 
}

Arg :: {
  (Maybe (Int, Int), Arg (Maybe (Int, Int)))
}
: ArgMod Ident ':' FullType {
  (fst $1, Tyche.Abs.Arg (fst $1)(snd $1)(snd $2)(snd $4)) 
}

ListArg :: {
  (Maybe (Int, Int), [Arg (Maybe (Int, Int))]) 
}
: {
  (Nothing, [])
}
| Arg {
  (fst $1, (:[]) (snd $1)) 
}
| Arg ',' ListArg {
  (fst $1, (:) (snd $1)(snd $3)) 
}

Stmt :: {
  (Maybe (Int, Int), Stmt (Maybe (Int, Int)))
}
: 'skip' {
  (Just (tokenLineCol $1), Tyche.Abs.Skip (Just (tokenLineCol $1)))
}
| 'break' {
  (Just (tokenLineCol $1), Tyche.Abs.Break (Just (tokenLineCol $1)))
}
| 'continue' {
  (Just (tokenLineCol $1), Tyche.Abs.Continue (Just (tokenLineCol $1)))
}
| 'return' Expr {
  (Just (tokenLineCol $1), Tyche.Abs.Ret (Just (tokenLineCol $1)) (snd $2)) 
}
| 'def' Ident ':' FullType '=' Expr {
  (Just (tokenLineCol $1), Tyche.Abs.VarDef (Just (tokenLineCol $1)) (snd $2)(snd $4)(snd $6)) 
}
| Ident '=' Expr {
  (fst $1, Tyche.Abs.Ass (fst $1)(snd $1)(snd $3)) 
}
| 'def' Ident ':' FullType '(' ListArg ')' 'do' '{' ListStmt '}' {
  (Just (tokenLineCol $1), Tyche.Abs.FnDef (Just (tokenLineCol $1)) (snd $2)(snd $4)(snd $6)(snd $10)) 
}
| Expr9 '(' ListExpr ')' {
  (fst $1, Tyche.Abs.FnApp (fst $1)(snd $1)(snd $3)) 
}
| 'if' Expr 'do' '{' ListStmt '}' {
  (Just (tokenLineCol $1), Tyche.Abs.Cond (Just (tokenLineCol $1)) (snd $2)(snd $5)) 
}
| 'if' Expr 'do' '{' ListStmt '}' 'else' 'do' '{' ListStmt '}' {
  (Just (tokenLineCol $1), Tyche.Abs.CondElse (Just (tokenLineCol $1)) (snd $2)(snd $5)(snd $10)) 
}
| 'while' Expr 'do' '{' ListStmt '}' {
  (Just (tokenLineCol $1), Tyche.Abs.While (Just (tokenLineCol $1)) (snd $2)(snd $5)) 
}
| 'for' 'each' Ident 'from' Expr 'do' '{' ListStmt '}' {
  (Just (tokenLineCol $1), Tyche.Abs.ForList (Just (tokenLineCol $1)) (snd $3)(snd $5)(snd $8)) 
}
| 'for' Ident 'from' Expr 'to' Expr 'do' '{' ListStmt '}' {
  (Just (tokenLineCol $1), Tyche.Abs.ForRange (Just (tokenLineCol $1)) (snd $2)(snd $4)(snd $6)(snd $9)) 
}

ListStmt :: {
  (Maybe (Int, Int), [Stmt (Maybe (Int, Int))]) 
}
: Stmt {
  (fst $1, (:[]) (snd $1)) 
}
| Stmt ';' ListStmt {
  (fst $1, (:) (snd $1)(snd $3)) 
}

Type :: {
  (Maybe (Int, Int), Type (Maybe (Int, Int)))
}
: 'int' {
  (Just (tokenLineCol $1), Tyche.Abs.Int (Just (tokenLineCol $1)))
}
| 'string' {
  (Just (tokenLineCol $1), Tyche.Abs.Str (Just (tokenLineCol $1)))
}
| 'boolean' {
  (Just (tokenLineCol $1), Tyche.Abs.Bool (Just (tokenLineCol $1)))
}
| 'void' {
  (Just (tokenLineCol $1), Tyche.Abs.Void (Just (tokenLineCol $1)))
}
| 'float' {
  (Just (tokenLineCol $1), Tyche.Abs.Float (Just (tokenLineCol $1)))
}
| '[' FullType ']' {
  (Just (tokenLineCol $1), Tyche.Abs.List (Just (tokenLineCol $1)) (snd $2)) 
}
| '{' FullType '}' {
  (Just (tokenLineCol $1), Tyche.Abs.Array (Just (tokenLineCol $1)) (snd $2)) 
}
| '(' ListArgType ')' '->' FullType {
  (Just (tokenLineCol $1), Tyche.Abs.Fun (Just (tokenLineCol $1)) (snd $2)(snd $5)) 
}

ArgType :: {
  (Maybe (Int, Int), ArgType (Maybe (Int, Int)))
}
: ArgMod FullType {
  (fst $1, Tyche.Abs.ArgType (fst $1)(snd $1)(snd $2)) 
}

FullType :: {
  (Maybe (Int, Int), FullType (Maybe (Int, Int)))
}
: ListTypeMod Type {
  (fst $1, Tyche.Abs.FullType (fst $1)(reverse (snd $1)) (snd $2)) 
}

ListArgType :: {
  (Maybe (Int, Int), [ArgType (Maybe (Int, Int))]) 
}
: {
  (Nothing, [])
}
| ArgType {
  (fst $1, (:[]) (snd $1)) 
}
| ArgType ',' ListArgType {
  (fst $1, (:) (snd $1)(snd $3)) 
}

ArgMod :: {
  (Maybe (Int, Int), ArgMod (Maybe (Int, Int)))
}
: 'var' {
  (Just (tokenLineCol $1), Tyche.Abs.AModVar (Just (tokenLineCol $1)))
}
| 'val' {
  (Just (tokenLineCol $1), Tyche.Abs.AModVal (Just (tokenLineCol $1)))
}
| 'inout' {
  (Just (tokenLineCol $1), Tyche.Abs.AModInOut (Just (tokenLineCol $1)))
}

TypeMod :: {
  (Maybe (Int, Int), TypeMod (Maybe (Int, Int)))
}
: 'readonly' {
  (Just (tokenLineCol $1), Tyche.Abs.TModReadonly (Just (tokenLineCol $1)))
}

ListTypeMod :: {
  (Maybe (Int, Int), [TypeMod (Maybe (Int, Int))]) 
}
: {
  (Nothing, [])
}
| ListTypeMod TypeMod {
  (fst $1, flip (:) (snd $1)(snd $2)) 
}

Expr9 :: {
  (Maybe (Int, Int), Expr (Maybe (Int, Int)))
}
: 'void' {
  (Just (tokenLineCol $1), Tyche.Abs.ELitVoid (Just (tokenLineCol $1)))
}
| Ident {
  (fst $1, Tyche.Abs.EVar (fst $1)(snd $1)) 
}
| Integer {
  (fst $1, Tyche.Abs.ELitInt (fst $1)(snd $1)) 
}
| 'true' {
  (Just (tokenLineCol $1), Tyche.Abs.ELitTrue (Just (tokenLineCol $1)))
}
| 'false' {
  (Just (tokenLineCol $1), Tyche.Abs.ELitFalse (Just (tokenLineCol $1)))
}
| String {
  (fst $1, Tyche.Abs.EString (fst $1)(snd $1)) 
}
| Double {
  (fst $1, Tyche.Abs.ELitFloat (fst $1)(snd $1)) 
}
| '[]' ':' FullType {
  (Just (tokenLineCol $1), Tyche.Abs.EEmpList (Just (tokenLineCol $1)) (snd $3)) 
}
| '{}' ':' FullType {
  (Just (tokenLineCol $1), Tyche.Abs.EEmpArray (Just (tokenLineCol $1)) (snd $3)) 
}
| Expr9 '(' ListExpr ')' {
  (fst $1, Tyche.Abs.EApp (fst $1)(snd $1)(snd $3)) 
}
| Expr9 '[' Expr ']' {
  (fst $1, Tyche.Abs.EArrApp (fst $1)(snd $1)(snd $3)) 
}
| '(' Expr ')' {
  (Just (tokenLineCol $1), snd $2)
}

Expr8 :: {
  (Maybe (Int, Int), Expr (Maybe (Int, Int)))
}
: '-' Expr9 {
  (Just (tokenLineCol $1), Tyche.Abs.Neg (Just (tokenLineCol $1)) (snd $2)) 
}
| '!' Expr9 {
  (Just (tokenLineCol $1), Tyche.Abs.Not (Just (tokenLineCol $1)) (snd $2)) 
}
| 'not' Expr9 {
  (Just (tokenLineCol $1), Tyche.Abs.Not (Just (tokenLineCol $1)) (snd $2)) 
}
| Expr9 {
  (fst $1, snd $1)
}

Expr7 :: {
  (Maybe (Int, Int), Expr (Maybe (Int, Int)))
}
: Expr8 '.' Expr7 {
  (fst $1, Tyche.Abs.ECons (fst $1)(snd $1)(snd $3)) 
}
| Expr8 {
  (fst $1, snd $1)
}

Expr6 :: {
  (Maybe (Int, Int), Expr (Maybe (Int, Int)))
}
: Expr6 MulOp Expr7 {
  (fst $1, Tyche.Abs.EMul (fst $1)(snd $1)(snd $2)(snd $3)) 
}
| Expr7 {
  (fst $1, snd $1)
}

Expr5 :: {
  (Maybe (Int, Int), Expr (Maybe (Int, Int)))
}
: Expr5 AddOp Expr6 {
  (fst $1, Tyche.Abs.EAdd (fst $1)(snd $1)(snd $2)(snd $3)) 
}
| Expr6 {
  (fst $1, snd $1)
}

Expr4 :: {
  (Maybe (Int, Int), Expr (Maybe (Int, Int)))
}
: Expr4 RelOp Expr5 {
  (fst $1, Tyche.Abs.ERel (fst $1)(snd $1)(snd $2)(snd $3)) 
}
| Expr5 {
  (fst $1, snd $1)
}

Expr3 :: {
  (Maybe (Int, Int), Expr (Maybe (Int, Int)))
}
: Expr4 AndOp Expr3 {
  (fst $1, Tyche.Abs.EAnd (fst $1)(snd $1)(snd $2)(snd $3)) 
}
| Expr4 {
  (fst $1, snd $1)
}

Expr2 :: {
  (Maybe (Int, Int), Expr (Maybe (Int, Int)))
}
: Expr3 OrOp Expr2 {
  (fst $1, Tyche.Abs.EOr (fst $1)(snd $1)(snd $2)(snd $3)) 
}
| Expr3 {
  (fst $1, snd $1)
}

Expr1 :: {
  (Maybe (Int, Int), Expr (Maybe (Int, Int)))
}
: '[' ListExpr ']' {
  (Just (tokenLineCol $1), Tyche.Abs.EList (Just (tokenLineCol $1)) (snd $2)) 
}
| '{' ListExpr '}' {
  (Just (tokenLineCol $1), Tyche.Abs.EArr (Just (tokenLineCol $1)) (snd $2)) 
}
| 'array' Expr 'times' Expr ':' FullType {
  (Just (tokenLineCol $1), Tyche.Abs.EArrSize (Just (tokenLineCol $1)) (snd $2)(snd $4)(snd $6)) 
}
| Expr2 {
  (fst $1, snd $1)
}

Expr :: {
  (Maybe (Int, Int), Expr (Maybe (Int, Int)))
}
: Expr1 '?' Expr1 ':' Expr1 {
  (fst $1, Tyche.Abs.EIf (fst $1)(snd $1)(snd $3)(snd $5)) 
}
| 'lambda' ':' FullType '(' ListArg ')' 'do' '{' ListStmt '}' {
  (Just (tokenLineCol $1), Tyche.Abs.ELambda (Just (tokenLineCol $1)) (snd $3)(snd $5)(snd $9)) 
}
| 'random' 'from' Expr1 {
  (Just (tokenLineCol $1), Tyche.Abs.ERand (Just (tokenLineCol $1)) (snd $3)) 
}
| 'probability' 'sampled' Expr1 'times' 'of' '{' ListStmt '}' 'satisfying' Expr1 {
  (Just (tokenLineCol $1), Tyche.Abs.EProbSamp (Just (tokenLineCol $1)) (snd $3)(snd $7)(snd $10)) 
}
| Expr1 {
  (fst $1, snd $1)
}

ListExpr :: {
  (Maybe (Int, Int), [Expr (Maybe (Int, Int))]) 
}
: {
  (Nothing, [])
}
| Expr {
  (fst $1, (:[]) (snd $1)) 
}
| Expr ',' ListExpr {
  (fst $1, (:) (snd $1)(snd $3)) 
}

AddOp :: {
  (Maybe (Int, Int), AddOp (Maybe (Int, Int)))
}
: '+' {
  (Just (tokenLineCol $1), Tyche.Abs.Plus (Just (tokenLineCol $1)))
}
| '-' {
  (Just (tokenLineCol $1), Tyche.Abs.Minus (Just (tokenLineCol $1)))
}

MulOp :: {
  (Maybe (Int, Int), MulOp (Maybe (Int, Int)))
}
: '*' {
  (Just (tokenLineCol $1), Tyche.Abs.Times (Just (tokenLineCol $1)))
}
| '/' {
  (Just (tokenLineCol $1), Tyche.Abs.Div (Just (tokenLineCol $1)))
}
| '%' {
  (Just (tokenLineCol $1), Tyche.Abs.Mod (Just (tokenLineCol $1)))
}
| 'mod' {
  (Just (tokenLineCol $1), Tyche.Abs.Mod (Just (tokenLineCol $1)))
}

RelOp :: {
  (Maybe (Int, Int), RelOp (Maybe (Int, Int)))
}
: '<' {
  (Just (tokenLineCol $1), Tyche.Abs.LTH (Just (tokenLineCol $1)))
}
| '<=' {
  (Just (tokenLineCol $1), Tyche.Abs.LE (Just (tokenLineCol $1)))
}
| '>' {
  (Just (tokenLineCol $1), Tyche.Abs.GTH (Just (tokenLineCol $1)))
}
| '>=' {
  (Just (tokenLineCol $1), Tyche.Abs.GE (Just (tokenLineCol $1)))
}
| '==' {
  (Just (tokenLineCol $1), Tyche.Abs.EQU (Just (tokenLineCol $1)))
}
| 'equals' {
  (Just (tokenLineCol $1), Tyche.Abs.EQU (Just (tokenLineCol $1)))
}
| '!=' {
  (Just (tokenLineCol $1), Tyche.Abs.NE (Just (tokenLineCol $1)))
}

OrOp :: {
  (Maybe (Int, Int), OrOp (Maybe (Int, Int)))
}
: 'or' {
  (Just (tokenLineCol $1), Tyche.Abs.Or (Just (tokenLineCol $1)))
}
| '||' {
  (Just (tokenLineCol $1), Tyche.Abs.Or (Just (tokenLineCol $1)))
}

AndOp :: {
  (Maybe (Int, Int), AndOp (Maybe (Int, Int)))
}
: 'and' {
  (Just (tokenLineCol $1), Tyche.Abs.And (Just (tokenLineCol $1)))
}
| '&&' {
  (Just (tokenLineCol $1), Tyche.Abs.And (Just (tokenLineCol $1)))
}

{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    t:_ -> " before `" ++ id(prToken t) ++ "'"

myLexer = tokens

pProgram = (>>= return . snd) . pProgram_internal
pArg = (>>= return . snd) . pArg_internal
pListArg = (>>= return . snd) . pListArg_internal
pStmt = (>>= return . snd) . pStmt_internal
pListStmt = (>>= return . snd) . pListStmt_internal
pType = (>>= return . snd) . pType_internal
pArgType = (>>= return . snd) . pArgType_internal
pFullType = (>>= return . snd) . pFullType_internal
pListArgType = (>>= return . snd) . pListArgType_internal
pArgMod = (>>= return . snd) . pArgMod_internal
pTypeMod = (>>= return . snd) . pTypeMod_internal
pListTypeMod = (>>= return . snd) . pListTypeMod_internal
pExpr9 = (>>= return . snd) . pExpr9_internal
pExpr8 = (>>= return . snd) . pExpr8_internal
pExpr7 = (>>= return . snd) . pExpr7_internal
pExpr6 = (>>= return . snd) . pExpr6_internal
pExpr5 = (>>= return . snd) . pExpr5_internal
pExpr4 = (>>= return . snd) . pExpr4_internal
pExpr3 = (>>= return . snd) . pExpr3_internal
pExpr2 = (>>= return . snd) . pExpr2_internal
pExpr1 = (>>= return . snd) . pExpr1_internal
pExpr = (>>= return . snd) . pExpr_internal
pListExpr = (>>= return . snd) . pListExpr_internal
pAddOp = (>>= return . snd) . pAddOp_internal
pMulOp = (>>= return . snd) . pMulOp_internal
pRelOp = (>>= return . snd) . pRelOp_internal
pOrOp = (>>= return . snd) . pOrOp_internal
pAndOp = (>>= return . snd) . pAndOp_internal
}

