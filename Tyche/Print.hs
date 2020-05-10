{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Tyche.Print where

-- pretty-printer generated by the BNF converter

import Tyche.Abs
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else ' ':s)

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)


instance Print Ident where
  prt _ (Ident i) = doc (showString ( i))



instance Print (Program a) where
  prt i e = case e of
    Program _ stmt -> prPrec i 0 (concatD [prt 0 stmt])

instance Print (Arg a) where
  prt i e = case e of
    Arg _ argmod fullident fulltype -> prPrec i 0 (concatD [prt 0 argmod, prt 0 fullident, doc (showString ":"), prt 0 fulltype])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print (FullIdent a) where
  prt i e = case e of
    FullIdent _ id -> prPrec i 0 (concatD [prt 0 id])
    AnonIdent _ -> prPrec i 0 (concatD [doc (showString "_")])

instance Print (Stmt a) where
  prt i e = case e of
    Skip _ -> prPrec i 5 (concatD [doc (showString ";")])
    Break _ -> prPrec i 5 (concatD [doc (showString "break"), doc (showString ";")])
    Continue _ -> prPrec i 5 (concatD [doc (showString "continue"), doc (showString ";")])
    Ret _ expr -> prPrec i 5 (concatD [doc (showString "return"), prt 0 expr, doc (showString ";")])
    VRet _ -> prPrec i 5 (concatD [doc (showString "return"), doc (showString ";")])
    VarDef _ fullident fulltype expr -> prPrec i 5 (concatD [doc (showString "def"), prt 0 fullident, doc (showString ":"), prt 0 fulltype, doc (showString "="), prt 1 expr, doc (showString ";")])
    Ass _ id expr -> prPrec i 5 (concatD [prt 0 id, doc (showString "="), prt 1 expr, doc (showString ";")])
    FnDef _ fullident fulltype args stmt -> prPrec i 4 (concatD [doc (showString "def"), prt 0 fullident, doc (showString ":"), prt 0 fulltype, doc (showString "("), prt 0 args, doc (showString ")"), prt 5 stmt])
    Cond _ expr stmt -> prPrec i 1 (concatD [doc (showString "if"), prt 0 expr, doc (showString "then"), prt 1 stmt])
    CondElse _ expr stmt1 stmt2 -> prPrec i 1 (concatD [doc (showString "if"), prt 0 expr, doc (showString "then"), prt 1 stmt1, doc (showString "else"), prt 1 stmt2])
    While _ expr stmt -> prPrec i 1 (concatD [doc (showString "while"), prt 0 expr, doc (showString "do"), prt 1 stmt])
    ForList _ id expr stmt -> prPrec i 1 (concatD [doc (showString "for"), doc (showString "each"), prt 0 id, doc (showString "from"), prt 0 expr, doc (showString "do"), prt 1 stmt])
    ForRange _ id expr1 expr2 stmt -> prPrec i 1 (concatD [doc (showString "for"), prt 0 id, doc (showString "from"), prt 0 expr1, doc (showString "to"), prt 0 expr2, doc (showString "do"), prt 1 stmt])
    Composition _ stmt1 stmt2 -> prPrec i 0 (concatD [prt 1 stmt1, prt 0 stmt2])

instance Print (Type a) where
  prt i e = case e of
    Int _ -> prPrec i 0 (concatD [doc (showString "int")])
    Str _ -> prPrec i 0 (concatD [doc (showString "string")])
    Bool _ -> prPrec i 0 (concatD [doc (showString "boolean")])
    Void _ -> prPrec i 0 (concatD [doc (showString "void")])
    Float _ -> prPrec i 0 (concatD [doc (showString "float")])
    List _ fulltype -> prPrec i 0 (concatD [doc (showString "("), prt 0 fulltype, doc (showString ")")])
    Array _ fulltype -> prPrec i 0 (concatD [doc (showString "["), prt 0 fulltype, doc (showString "]")])
    Fun _ argtypes fulltype -> prPrec i 0 (concatD [doc (showString "("), prt 0 argtypes, doc (showString ")"), doc (showString "->"), prt 0 fulltype])

instance Print (ArgType a) where
  prt i e = case e of
    ArgType _ argmod fulltype -> prPrec i 0 (concatD [prt 0 argmod, prt 0 fulltype])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print (FullType a) where
  prt i e = case e of
    FullType _ typemods type_ -> prPrec i 0 (concatD [prt 0 typemods, prt 0 type_])

instance Print (ArgMod a) where
  prt i e = case e of
    AModVar _ -> prPrec i 0 (concatD [doc (showString "var")])
    AModVal _ -> prPrec i 0 (concatD [doc (showString "val")])
    AModInOut _ -> prPrec i 0 (concatD [doc (showString "inout")])

instance Print (TypeMod a) where
  prt i e = case e of
    TModReadonly _ -> prPrec i 0 (concatD [doc (showString "readonly")])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print (Expr a) where
  prt i e = case e of
    EVar _ id -> prPrec i 9 (concatD [prt 0 id])
    ELitInt _ n -> prPrec i 9 (concatD [prt 0 n])
    ELitTrue _ -> prPrec i 9 (concatD [doc (showString "true")])
    ELitFalse _ -> prPrec i 9 (concatD [doc (showString "false")])
    EString _ str -> prPrec i 9 (concatD [prt 0 str])
    ELitFloat _ d -> prPrec i 9 (concatD [prt 0 d])
    EEmpList _ fulltype -> prPrec i 9 (concatD [doc (showString "()"), doc (showString ":"), prt 0 fulltype])
    Neg _ expr -> prPrec i 8 (concatD [doc (showString "-"), prt 9 expr])
    Not _ expr -> prPrec i 8 (concatD [doc (showString "!"), prt 9 expr])
    ECons _ expr1 expr2 -> prPrec i 7 (concatD [prt 8 expr1, doc (showString "::"), prt 7 expr2])
    EMul _ expr1 mulop expr2 -> prPrec i 6 (concatD [prt 6 expr1, prt 0 mulop, prt 7 expr2])
    EAdd _ expr1 addop expr2 -> prPrec i 5 (concatD [prt 5 expr1, prt 0 addop, prt 6 expr2])
    ERel _ expr1 relop expr2 -> prPrec i 4 (concatD [prt 4 expr1, prt 0 relop, prt 5 expr2])
    EAnd _ expr1 andop expr2 -> prPrec i 3 (concatD [prt 4 expr1, prt 0 andop, prt 3 expr2])
    EOr _ expr1 orop expr2 -> prPrec i 2 (concatD [prt 3 expr1, prt 0 orop, prt 2 expr2])
    EList _ exprs -> prPrec i 1 (concatD [doc (showString "("), prt 0 exprs, doc (showString ")")])
    EArr _ exprs -> prPrec i 1 (concatD [doc (showString "["), prt 0 exprs, doc (showString "]")])
    EArrSize _ fulltype expr -> prPrec i 1 (concatD [doc (showString "array"), doc (showString ":"), prt 0 fulltype, doc (showString "["), prt 0 expr, doc (showString "]")])
    EApp _ expr exprs -> prPrec i 1 (concatD [prt 2 expr, doc (showString "("), prt 0 exprs, doc (showString ")")])
    EArrApp _ expr1 expr2 -> prPrec i 1 (concatD [prt 2 expr1, doc (showString "["), prt 0 expr2, doc (showString "]")])
    EIf _ expr1 expr2 expr3 -> prPrec i 0 (concatD [prt 1 expr1, doc (showString "?"), prt 1 expr2, doc (showString ":"), prt 1 expr3])
    ELambda _ fulltype args stmt -> prPrec i 0 (concatD [doc (showString "lambda"), doc (showString ":"), prt 0 fulltype, doc (showString "("), prt 0 args, doc (showString ")"), doc (showString "->"), prt 1 stmt])
    ERand _ expr -> prPrec i 0 (concatD [doc (showString "random"), doc (showString "from"), prt 1 expr])
    ERandDist _ expr1 expr2 -> prPrec i 0 (concatD [doc (showString "random"), doc (showString "from"), prt 1 expr1, doc (showString "distribution"), prt 1 expr2])
    EProb _ stmt expr -> prPrec i 0 (concatD [doc (showString "probability"), doc (showString "of"), prt 1 stmt, doc (showString "satisfying"), prt 1 expr])
    EProbSamp _ expr1 stmt expr2 -> prPrec i 0 (concatD [doc (showString "probability"), doc (showString "tested"), prt 1 expr1, doc (showString "times"), doc (showString "of"), prt 1 stmt, doc (showString "satisfying"), prt 1 expr2])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print (AddOp a) where
  prt i e = case e of
    Plus _ -> prPrec i 0 (concatD [doc (showString "+")])
    Minus _ -> prPrec i 0 (concatD [doc (showString "-")])

instance Print (MulOp a) where
  prt i e = case e of
    Times _ -> prPrec i 0 (concatD [doc (showString "*")])
    Div _ -> prPrec i 0 (concatD [doc (showString "/")])
    Mod _ -> prPrec i 0 (concatD [doc (showString "%")])

instance Print (RelOp a) where
  prt i e = case e of
    LTH _ -> prPrec i 0 (concatD [doc (showString "<")])
    LE _ -> prPrec i 0 (concatD [doc (showString "<=")])
    GTH _ -> prPrec i 0 (concatD [doc (showString ">")])
    GE _ -> prPrec i 0 (concatD [doc (showString ">=")])
    EQU _ -> prPrec i 0 (concatD [doc (showString "==")])
    NE _ -> prPrec i 0 (concatD [doc (showString "!=")])

instance Print (OrOp a) where
  prt i e = case e of
    Or _ -> prPrec i 0 (concatD [doc (showString "or")])

instance Print (AndOp a) where
  prt i e = case e of
    And _ -> prPrec i 0 (concatD [doc (showString "and")])


