{-
    BNF Converter: C++ Bison generator
    Copyright (C) 2004  Author:  Michael Pellauer

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{-
   **************************************************************
    BNF Converter Module

    Description   : This module generates the Bison input file using
                    STL. The main difference to CFtoBison is in handling
                    lists: by using std::vector and push_back, our rules
                    for reverting lists are the opposite to linked lists.
                    Note that because of the way bison stores results
                    the programmer can increase performance by limiting
                    the number of entry points in their grammar.

    Author        : Michael Pellauer (pellauer@cs.chalmers.se)

    License       : GPL (GNU General Public License)

    Created       : 6 August, 2003

    Modified      : 19 August, 2006, by Aarne Ranta (aarne@cs.chalmers.se)


   **************************************************************
-}


module BNFC.Backend.CPP.STL.CFtoBisonSTL (cf2Bison, union) where

import Prelude hiding((<>))

import Data.Char (toLower,isUpper)
import Data.List (nub, intercalate)
import Data.Maybe (fromMaybe)

import BNFC.Backend.C.CFtoBisonC (startSymbol)
import BNFC.Backend.CPP.STL.STLUtils
import BNFC.Backend.Common.NamedVariables hiding (varName)
import BNFC.CF
import BNFC.PrettyPrint
import BNFC.TypeChecker
import BNFC.Utils ((+++))
import ErrM

--This follows the basic structure of CFtoHappy.

-- Type declarations
type Rules       = [(NonTerminal,[(Pattern,Action)])]
type Pattern     = String
type Action      = String
type MetaVar     = String

--The environment comes from the CFtoFlex
cf2Bison :: Bool -> Maybe String -> String -> CF -> SymEnv -> String
cf2Bison ln inPackage name cf env
 = unlines
    [header inPackage name cf,
     render $ union inPackage (positionCats cf ++ allCats cf),
     maybe "" (\ns -> "%define api.prefix {" ++ ns ++ "yy}") inPackage,
     "%token _ERROR_",
     tokens user env,
     declarations cf,
     startSymbol cf,
     specialToks cf,
     "%%",
     prRules (rulesForBison ln inPackage name cf env)
    ]
  where
   user = fst (unzip (tokenPragmas cf))


positionCats cf = filter (isPositionCat cf) $ fst (unzip (tokenPragmas cf))

header :: Maybe String -> String -> CF -> String
header inPackage name cf = unlines
    [ "/* This Bison file was machine-generated by BNFC */"
    , "%{"
    , "#include <stdlib.h>"
    , "#include <stdio.h>"
    , "#include <string.h>"
    , "#include <iostream>"
    , "#include <algorithm>"
    , "#include \"Absyn.H\""
    , "typedef struct yy_buffer_state *YY_BUFFER_STATE;"
    , "int yyparse(void);"
    , "int yylex(void);"
    , "YY_BUFFER_STATE " ++ ns ++ "yy_scan_string(const char *str);"
    , "void " ++ ns ++ "yy_delete_buffer(YY_BUFFER_STATE buf);"
    , "int " ++ ns ++ "yy_mylinenumber;"  --- hack to get line number. AR 2006
    , "int " ++ ns ++ "initialize_lexer(FILE * inp);"
    , "int " ++ ns ++ "yywrap(void)"
    , "{"
    , "  return 1;"
    , "}"
    , "void " ++ ns ++ "yyerror(const char *str)"
    , "{"
    , "  extern char *"++ns++"yytext;"
    , "  fprintf(stderr,\"error: line %d: %s at %s\\n\", "
    , "    "++ns++"yy_mylinenumber, str, "++ns++"yytext);"
    , "}"
    , ""
    , definedRules cf
    , nsStart inPackage
    , unlines $ map (parseMethod inPackage name) (allCatsNorm cf ++ positionCats cf)  -- (allEntryPoints cf), M.F. 2004-09-14 fix of [Ty2] bug.
    , nsEnd inPackage
    , "%}"
    ]
  where
   ns = nsString inPackage

definedRules :: CF -> String
definedRules cf =
    unlines [ rule f xs e | FunDef f xs e <- cfgPragmas cf ]
  where
    ctx = buildContext cf

    list = LC (const "[]") (\t -> "List" ++ unBase t)
      where
        unBase (ListT t) = unBase t
        unBase (BaseT x) = show $ normCat $ strToCat x

    rule f xs e =
        case checkDefinition' list ctx f xs e of
        Bad err -> error $ "Panic! This should have been caught already:\n" ++ err
        Ok (args,(e',t)) -> unlines
            [ cppType t ++ " " ++ f ++ "_ (" ++
                intercalate ", " (map cppArg args) ++ ") {"
            , "  return " ++ cppExp e' ++ ";"
            , "}"
            ]
      where
        cppType :: Base -> String
        cppType (ListT (BaseT x)) = "List" ++ show (normCat $ strToCat x) ++ " *"
        cppType (ListT t)         = cppType t ++ " *"
        cppType (BaseT x)
            | isToken x ctx = "String"
            | otherwise     = show (normCat $ strToCat x) ++ " *"

        cppArg :: (String, Base) -> String
        cppArg (x,t) = cppType t ++ " " ++ x ++ "_"

        cppExp :: Exp -> String
        cppExp (App "[]" []) = "0"
        cppExp (App x [])
            | x `elem` xs         = x ++ "_"  -- argument
        cppExp (App t [e])
            | isToken t ctx     = cppExp e
        cppExp (App x es)
            | isUpper (head x)  = call ("new " ++ x) es
            | otherwise         = call (x ++ "_") es
        cppExp (LitInt n)       = show n
        cppExp (LitDouble x)    = show x
        cppExp (LitChar c)      = show c
        cppExp (LitString s)    = show s

        call x es = x ++ "(" ++ intercalate ", " (map cppExp es) ++ ")"


--This generates a parser method for each entry point.
parseMethod :: Maybe String -> String -> Cat -> String
parseMethod inPackage _ cat =
  -- if normCat cat /= cat     M.F. 2004-09-17 comment. No duplicates from allCatsIdNorm
  -- then ""
  -- else
  unlines
  [
   "static " ++ cat' ++ "*" +++ resultName cat' +++ "= 0;",
   cat' ++"* p" ++ cat' ++ "(FILE *inp)",
   "{",
   "  " ++ ns ++ "yy_mylinenumber = 1;",       -- O.F.
   "  " ++ ns ++ "initialize_lexer(inp);",
   "  if (yyparse())",
   "  { /* Failure */",
   "    return 0;",
   "  }",
   "  else",
   "  { /* Success */",
   "    return" +++ resultName cat' ++ ";",
   "  }",
   "}",
   cat' ++"* p" ++ cat' ++ "(const char *str)",
   "{",
   "  YY_BUFFER_STATE buf;",
   "  int result;",
   "  " ++ ns ++ "yy_mylinenumber = 1;",
   "  " ++ ns ++ "initialize_lexer(0);",
   "  buf = " ++ ns ++ "yy_scan_string(str);",
   "  result = yyparse();",
   "  " ++ ns ++ "yy_delete_buffer(buf);",
   "  if (result)",
   "  { /* Failure */",
   "    return 0;",
   "  }",
   "  else",
   "  { /* Success */",
   "    return" +++ resultName cat' ++ ";",
   "  }",
   "}"
  ]
 where
  cat' = identCat (normCat cat)
  ns = nsString inPackage


-- | The union declaration is special to Bison/Yacc and gives the type of
-- yylval.  For efficiency, we may want to only include used categories here.
--
-- >>> let foo = Cat "Foo"
-- >>> union Nothing [foo, ListCat foo]
-- %union
-- {
--   int int_;
--   char char_;
--   double double_;
--   char* string_;
--   Foo* foo_;
--   ListFoo* listfoo_;
-- }
--
-- If the given list of categories is contains coerced categories, those should
-- be normalized and duplicate removed
-- E.g. if there is both [Foo] and [Foo2] we should only print one pointer:
--    ListFoo* listfoo_;
--
-- >>> let foo2 = CoercCat "Foo" 2
-- >>> union Nothing [foo, ListCat foo, foo2, ListCat foo2]
-- %union
-- {
--   int int_;
--   char char_;
--   double double_;
--   char* string_;
--   Foo* foo_;
--   ListFoo* listfoo_;
-- }
union :: Maybe String -> [Cat] -> Doc
union inPackage cats =
    "%union" $$ codeblock 2 (
        [ "int int_;"
        , "char char_;"
        , "double double_;"
        , "char* string_;" ]
        ++ map mkPointer normCats )
  where
    normCats = nub (map normCat cats)
    mkPointer s = scope <> text (identCat s) <> "*" <+> text (varName s) <> ";"
    scope = text (nsScope inPackage)

--declares non-terminal types.
declarations :: CF -> String
declarations cf = concatMap (typeNT cf) (positionCats cf ++ allCats cf)
 where --don't define internal rules
   typeNT cf nt | isPositionCat cf nt || rulesForCat cf nt /= [] =
      "%type <" ++ varName nt ++ "> " ++ identCat nt ++ "\n"
   typeNT _ _ = ""

--declares terminal types.
tokens :: [UserDef] -> SymEnv -> String
tokens user = concatMap (declTok user)
 where
  declTok u (s,r) = if s `elem` map show u
    then "%token<string_> " ++ r ++ "    //   " ++ s ++ "\n"
    else "%token " ++ r ++ "    //   " ++ s ++ "\n"

specialToks :: CF -> String
specialToks cf = concat [
  ifC catString "%token<string_> _STRING_\n",
  ifC catChar "%token<char_> _CHAR_\n",
  ifC catInteger "%token<int_> _INTEGER_\n",
  ifC catDouble "%token<double_> _DOUBLE_\n",
  ifC catIdent "%token<string_> _IDENT_\n"
  ]
   where
    ifC cat s = if isUsedCat cf cat then s else ""

--The following functions are a (relatively) straightforward translation
--of the ones in CFtoHappy.hs
rulesForBison :: Bool -> Maybe String -> String -> CF -> SymEnv -> Rules
rulesForBison ln inPackage _ cf env = map mkOne (ruleGroups cf) ++ posRules where
  mkOne (cat,rules) = constructRule ln inPackage cf env rules cat
  posRules = map mkPos $ positionCats cf
  mkPos cat = (cat, [(fromMaybe (show cat) (lookup (show cat) env),
   "$$ = new " ++ show cat ++ "($1," ++ nsString inPackage ++ "yy_mylinenumber) ; YY_RESULT_" ++
   show cat ++ "_= $$ ;")])

-- For every non-terminal, we construct a set of rules.
constructRule ::
  Bool -> Maybe String -> CF -> SymEnv -> [Rule] -> NonTerminal -> (NonTerminal,[(Pattern,Action)])
constructRule ln inPackage cf env rules nt =
  (nt,[(p, generateAction ln inPackage nt (ruleName r) b m +++ result) |
     r0 <- rules,
     let (b,r) = if isConsFun (funRule r0) && elem (valCat r0) revs
                   then (True,revSepListRule r0)
                 else (False,r0),
     let (p,m) = generatePatterns cf env r b])
 where
   ruleName r = case funRule r of
     ---- "(:)" -> identCat nt
     ---- "(:[])" -> identCat nt
     z -> z
   revs = cfgReversibleCats cf
   eps = allEntryPoints cf
   isEntry nt = nt `elem` eps
   result = if isEntry nt then (nsScope inPackage ++ resultName (identCat (normCat nt))) ++ "= $$;" else ""

-- Generates a string containing the semantic action.
generateAction :: Bool -> Maybe String -> NonTerminal -> Fun -> Bool -> [(MetaVar,Bool)] -> Action
generateAction ln inPackage cat f b mbs =
  reverses ++
  if isCoercion f
  then "$$ = " ++ unwords ms ++ ";"
  else if f == "[]"
  then concat ["$$ = ","new ", scope, identCatV cat, "();"]
  else if f == "(:[])"
  then concat ["$$ = ","new ", scope, identCatV cat, "() ; $$->push_back($1);"]
  else if f == "(:)" && b
  then "$1->push_back("++ lastms ++ ") ; $$ = $1 ;"
  else if f == "(:)"
  then lastms ++ "->push_back(" ++ head ms ++ ") ; $$ = " ++ lastms ++ " ;" ---- not left rec
  else if isDefinedRule f
  then concat ["$$ = ", scope, f, "_", "(", intercalate ", " ms, ");" ]
  else concat
    ["$$ = ", "new ", scope, f, "(", intercalate ", " ms, ");" ++ addLn ln]
 where
  ms = map fst mbs
  lastms = last ms
  addLn ln = if ln then " $$->line_number = " ++ nsString inPackage ++ "yy_mylinenumber;" else ""  -- O.F.
  identCatV = identCat . normCat
  reverses = unwords [
    "std::reverse(" ++ m ++"->begin(),"++m++"->end()) ;" |
       (m,True) <- mbs]
  scope = nsScope inPackage

-- Generate patterns and a set of metavariables indicating
-- where in the pattern the non-terminal
generatePatterns :: CF -> SymEnv -> Rule -> Bool -> (Pattern,[(MetaVar,Bool)])
generatePatterns cf env r _ = case rhsRule r of
  []  -> ("/* empty */",[])
  its -> (unwords (map mkIt its), metas its)
 where
   mkIt i = case i of
     Left c -> case lookup (show c) env of
       Just x | not (isPositionCat cf c) -> x
       _ -> typeName (identCat c)
     Right s -> fromMaybe s (lookup s env)
   metas its = [('$': show i,revert c) | (i,Left c) <- zip [1 :: Int ..] its]

   -- notice: reversibility with push_back vectors is the opposite
   -- of right-recursive lists!
   revert c = isList c && not (isConsFun (funRule r)) && notElem c revs
   revs = cfgReversibleCats cf

-- We have now constructed the patterns and actions,
-- so the only thing left is to merge them into one string.

prRules :: Rules -> String
prRules [] = []
prRules ((_, []):rs) = prRules rs --internal rule
prRules ((nt, (p, a) : ls):rs) =
    unwords [nt', ":" , p, "{ ", a, "}", "\n" ++ pr ls] ++ ";\n" ++ prRules rs
 where
  nt' = identCat nt
  pr []           = []
  pr ((p,a):ls)   = unlines [unwords ["  |", p, "{ ", a , "}"]] ++ pr ls

--Some helper functions.
resultName :: String -> String
resultName s = "YY_RESULT_" ++ s ++ "_"

--slightly stronger than the NamedVariable version.
varName :: Cat -> String
varName = (++ "_") . map toLower . identCat . normCat

typeName :: String -> String
typeName "Ident" = "_IDENT_"
typeName "String" = "_STRING_"
typeName "Char" = "_CHAR_"
typeName "Integer" = "_INTEGER_"
typeName "Double" = "_DOUBLE_"
typeName x = x
