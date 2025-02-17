{-
    BNF Converter: Happy Generator
    Copyright (C) 2004  Author:  Markus Forberg, Aarne Ranta

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

module BNFC.Backend.Haskell.CFtoHappy where

import Prelude hiding((<>))
import Data.String (IsString(..))

import BNFC.CF
import BNFC.Backend.Common.StrUtils (escapeChars)
import BNFC.Backend.Haskell.Utils (parserName)
import qualified BNFC.Backend.Haskell.AbsHappy as Happy
import BNFC.Backend.Haskell.PrintHappy (printTree)
import Data.Char (isLower)
import BNFC.Options (HappyMode(..))
import BNFC.PrettyPrint

-- default naming

tokenName   = "Token"

-- Happy mode



cf2HappyS :: String     -- ^ This module's name
          -> String     -- ^ Abstract syntax module name
          -> String     -- ^ Lexer module name
          -> String     -- ^ ErrM module name
          -> HappyMode  -- ^ Happy mode
          -> Bool       -- ^ Use bytestring?
          -> Bool       -- ^ AST is a functor?
          -> CF         -- ^ Grammar
          -> String     -- ^ Generated code
---- cf2HappyS :: String -> CF -> String
cf2HappyS = cf2Happy

-- The main function, that given a CF and a CFCat to parse according to,
-- generates a happy module.
cf2Happy name absName lexName errName mode byteStrings functor cf
 = unlines
    [header name absName lexName errName mode byteStrings,
    -- directives
     render $ declarations mode,
     render $ vcat $ map (mkParserName functor) (allEntryPoints cf),
     tokens (cfTokens cf),
     render $ nest 2 $ specialToks functor cf,
     delimiter,
     -- Productions
     render $ productions absName byteStrings functor cf,
     -- optional module trailer
     finalize byteStrings functor cf]

-- construct the header.
header :: String -> String -> String -> String -> HappyMode -> Bool -> String
header modName absName lexName errName mode byteStrings = unlines
         ["-- This Happy file was machine-generated by the BNF converter",
          "{",
          "{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}",
          case mode of
            Standard -> "module " ++ modName ++ " where"
            GLR      -> "-- module name filled in by Happy",
          "import " ++ absName,
          "import " ++ lexName,
          "import " ++ errName,
          if byteStrings then "import qualified Data.ByteString.Char8 as BS" else "",
          "}"
         ]

-- | The declarations of a happy file.
-- >>> declarations Standard
-- -- no lexer declaration
-- %monad { Err } { thenM } { returnM }
-- %tokentype {Token}
declarations :: HappyMode -> Doc
declarations mode = vcat
    [ case mode of
        Standard -> "-- no lexer declaration"
        GLR      -> "%lexer { myLexer } { Err _ }",
      "%monad { Err } { thenM } { returnM }",
      "%tokentype" <+> braces (text tokenName) ]

-- | Generate the parser name directive
-- See: https://www.haskell.org/happy/doc/html/sec-directives.html#sec-parser-name
-- Example:
-- %name pExp Exp
--
-- In case we use a functor, we need to generate internal names:
-- %name pExp_internal Exp
mkParserName :: Bool -> Cat -> Doc
mkParserName functor cat =
    "%name" <+> parserName cat <> (if functor then "_internal" else "")
    <+> text (identCat cat)


-- The useless delimiter symbol.
delimiter :: String
delimiter = "\n%%\n"

-- Generate the list of tokens and their identifiers.
tokens :: [(String,Int)] -> String
tokens toks = "%token\n" ++ prTokens toks
 where prTokens []         = []
       prTokens ((t,k):tk) = "  " ++ render (convert t) ++
                             " { " ++ oneTok t k ++ " }\n" ++
                             prTokens tk
       oneTok _ k = "PT _ (TS _ " ++ show k ++ ")"

-- Happy doesn't allow characters such as åäö to occur in the happy file. This
-- is however not a restriction, just a naming paradigm in the happy source file.
convert :: String -> Doc
convert = quotes . text . escapeChars


-- ------------------------------------------------------------------------- --
-- Productions
-- ------------------------------------------------------------------------- --

productions :: String -> Bool -> Bool -> CF -> Doc
productions absModule byteStrings functor cf =
    vcat $ map (text . printTree)
    (specialRules byteStrings functor cf
    ++
    map (mkProduction absModule functor reversibles) rulesForNonInternalCats)
  where
    rulesForNonInternalCats = [ x | x@(_, _:_) <- ruleGroups cf]
    reversibles = cfgReversibleCats cf


-- | Generate an expression that represent the semantic value of a non-terminal
-- according to a given rule.
action :: String -> Bool -> [Cat] -> Rule -> Happy.Expression
action absName functor reversibles r@(Rule label cat _) =
    if functor then Happy.EPair position node else node
  where
    position = getPosition rhs'
    node | isCoercion label = head metavars
         | otherwise = foldl Happy.EApp constructor metavars
    constructor
      | isConsFun label && elem cat reversibles = Happy.EApp "flip" (fromString label)
      | isNilCons label = fromString label
      | functor = Happy.EApp label' position
      | otherwise = label'
    label' | isDefinedRule label = fromString (absName ++ "." ++ label ++ "_")
           | otherwise = fromString (absName ++ "." ++ label)
    metavars = do
        (i, Left cat) <- zip [1..] rhs'
        return $
            (if not (isConsFun label) && elem cat reversibles
            then Happy.EApp "reverse" else id)
            $ (if functor then Happy.EApp "snd" else id)
            (mkMetaVariable i)
    Rule _ _ rhs' | isConsFun label && elem cat reversibles = revSepListRule r
                  | otherwise = r


-- | This function is used when bnfc is called with the --functor option.
-- It generates the code that computes the current node position from the
-- positions stored in the meta-variables.
getPosition :: [Either Cat String] -> Happy.Expression
getPosition rhs = case rhs of
    [] -> Happy.EIdent (Happy.Ident "Nothing")
    (Left _:_) -> Happy.EApp "fst" metavar
    (Right _:_) -> Happy.EApp "Just" (Happy.EApp "tokenLineCol" metavar)
  where
    metavar = mkMetaVariable 1

-- | Make a happy meta-variable, e.g. $2
mkMetaVariable :: Int -> Happy.Expression
mkMetaVariable = Happy.EIdent . Happy.Ident . ("$" ++) . show

-- Generate a pattern for the given rule. A pattern is a list of terminals and
-- non-terminals that goes in the right-hand side of a happy production.
mkPattern :: [Cat] -> Rule -> [Happy.Symbol]
mkPattern reversibles r@(Rule label cat _) = case rhs' of
    []  -> []
    items -> [mkSymbol i | i <- items]
  where
    mkSymbol (Left cat) = Happy.NonTerm (mkNonTerminal cat)
    mkSymbol (Right s) = Happy.QuotedTerm (mkTerminal s)
    Rule _ _ rhs' | isConsFun label && elem cat reversibles = revSepListRule r
                  | otherwise = r

mkNonTerminal :: Cat -> Happy.Ident
mkNonTerminal = Happy.Ident . identCat


-- | Create a terminal identifier for a bnfc terminal, i.e. a literal string
-- in a rule.
mkTerminal :: String -> Happy.Terminal
mkTerminal s = Happy.Terminal $ "'" ++ escapeChars s ++ "'"

-- | Create a terminal identifier for a toker category
mkTokenTerminal :: String -> Happy.Symbol
mkTokenTerminal cat = Happy.IdentTerm $ Happy.Ident $ case cat of
    "Ident"   -> "L_ident"
    "String"  -> "L_quoted"
    "Integer" -> "L_integ"
    "Double"  -> "L_doubl"
    "Char"    -> "L_charac"
    _       -> "L_" ++ cat

-- | Builds the return type of a production for a given category.
mkProductionType :: Bool -> Cat -> Happy.Type
mkProductionType functor cat
    | functor = Happy.TPair maybeIntIntT (catToType (Just maybeIntIntT) cat)
    | otherwise = catToType Nothing cat
  where
    maybeT = Happy.TIdent (Happy.Ident "Maybe")
    intT = Happy.TIdent (Happy.Ident "Int")
    maybeIntIntT = Happy.TApp maybeT (Happy.TPair intT intT)
    catToType :: Maybe Happy.Type -> Cat -> Happy.Type
    catToType _ InternalCat = error "Can't create a haskell type for internal category"
    catToType t (ListCat c) = Happy.TList (catToType t c)
    catToType (Just _) c@(TokenCat _) = catToType Nothing c
    catToType (Just t) c = Happy.TApp (catToType Nothing c) t
    catToType Nothing c = Happy.TIdent $ Happy.Ident $ show $ normCat c

-- | Generate a Happy Production for the given set of rules
mkProduction :: String -> Bool -> [Cat] -> (Cat, [Rule]) -> Happy.Production
mkProduction absMod functor reversibles (cat, rules) =
    Happy.P
      (mkNonTerminal cat)
      (mkProductionType functor cat)
      (map mkRhs rules)
  where
    mkRhs rule = Happy.Rhs
        (mkPattern reversibles rule)
        (action absMod functor reversibles rule)

-- Finally, some haskell code.

finalize :: Bool -> Bool -> CF -> String
finalize byteStrings functor cf = unlines $
   [
     "{",
     "\nreturnM :: a -> Err a",
     "returnM = return",
     "\nthenM :: Err a -> (a -> Err b) -> Err b",
     "thenM = (>>=)",
     "\nhappyError :: [" ++ tokenName ++ "] -> Err a",
     "happyError ts =",
     "  Bad $ \"syntax error at \" ++ tokenPos ts ++ ",
     "  case ts of",
     "    [] -> []",
     "    [Err _] -> \" due to lexer error\"",
     "    t:_ -> \" before `\" ++ " ++ stringUnpack ++ "(prToken t) ++ \"'\"",
     "",
     "myLexer = tokens",
     "",
     if functor
     then render $ vcat $ map mkParserFun (allEntryPoints cf)
     else ""
   ] ++ definedRules cf ++ [ "}" ]
   where
     stringUnpack
       | byteStrings = "BS.unpack"
       | otherwise   = "id"
     mkParserFun cat =
          parserName cat <+> "=" <+>  "(>>= return . snd)" <+> "." <+> parserName cat <> "_internal"


definedRules cf = [ mkDef f xs e | FunDef f xs e <- cfgPragmas cf ]
    where
        mkDef f xs e = unwords $ (f ++ "_") : xs' ++ ["=", show e']
            where
                xs' = map (++"_") xs
                e'  = underscore e
        underscore (App x es)
            | isLower $ head x  = App (x ++ "_") $ map underscore es
            | otherwise         = App x $ map underscore es
        underscore e          = e

-- aarne's modifs 8/1/2002:
-- Markus's modifs 11/02/2002

-- GF literals
specialToks :: Bool -> CF -> Doc
specialToks functor cf = vsep (map aux (literals cf))
 where aux cat = specialTok cat (isPositionCat cf cat || functor)


-- | Generate a Happy terminal symbol declaration for the BNFC token rules,
-- both built-in and user defined.
-- >>> specialTok (TokenCat "Ident") False
-- L_ident {PT _ (TV $$)}
--
-- >>> specialTok (TokenCat "Foo") False
-- L_Foo {PT _ (T_Foo $$)}
--
-- The boolean argument tells us if we need to keep the position, in which case
-- we cannot use the Happy $$ magic projection.
-- >>> specialTok (TokenCat "Bar") True
-- L_Bar {PT _ (T_Bar _)}
--
-- Note that we might also need to keep the position for built-in tokens:
-- >>> specialTok (TokenCat "Integer") True
-- L_integ {PT _ (TI _)}
specialTok :: Cat -> Bool -> Doc
specialTok (TokenCat cat) keepPos =
    terminalName cat <+> braces ("PT _" <+> parens (cons <+> posn))
  where
    cons = case cat of
        "Ident"   -> "TV"
        "String"  -> "TL"
        "Integer" -> "TI"
        "Double"  -> "TD"
        "Char"    -> "TC"
        own       -> "T_" <> text own
    posn = if keepPos then "_" else "$$"
specialTok _ _ = error "specialTok is only for TokenCat"


specialRules :: Bool -> Bool -> CF -> [Happy.Production]
specialRules byteStrings functor cf = map aux (literals cf)
 where
   aux cat = specialRule byteStrings (isPositionCat cf cat) functor cat


-- | Generate grammar rules for each kind of token in the LBNF grammar.
-- Rules for the built-in token types (Integer, String, Double...) return a
-- literate value in haskell (e.g. the rule for Integer will actually return
-- an integer).
specialRule :: Bool -> Bool -> Bool -> Cat -> Happy.Production
specialRule byteStrings isPositionToken keepPos c@(TokenCat cat) = Happy.P
    (mkNonTerminal c)
    (mkProductionType keepPos c)
    [ Happy.Rhs
        [mkTokenTerminal cat]
        (mkTokenAction isPositionToken byteStrings keepPos cat)]
  where
specialRule _ _ _ _ = error "specialRule is only for TokenCat"


-- | Generate the action (the expression in braces in the right-hand side of a
-- happy production) for Token categories
mkTokenAction :: Bool -> Bool -> Bool -> String -> Happy.Expression
mkTokenAction isPositionToken byteStrings functor catName =
    if functor then Happy.EPair position value
               else value
  where
    value = case catName of
        "String" ->  unpack (prToken metavar)
        _ | catName `elem` ["Integer", "Double", "Char"] ->
            Happy.EApp read (unpack (prToken metavar))
        _ -> Happy.EApp cons (mkPosToken (prToken metavar))
    -- generate a function application
    app :: String -> Happy.Expression -> Happy.Expression
    app f = Happy.EApp (Happy.EIdent (Happy.Ident f))
    position = app "Just" $ app "tokenLineCol" metavar
    cons = Happy.EIdent (Happy.Ident catName)
    read = Happy.EIdent (Happy.Ident "read")
    metavar = mkMetaVariable 1
    unpack | byteStrings = app "BS.unpack"
           | otherwise = id
    mkPosToken | isPositionToken = app "mkPosToken"
               | otherwise = id
    prToken | functor = app "prToken"
            | otherwise = id

-- | Generate a terminal name for a given category name that will be used to
-- link a pattern in the %token declarations and the grammar rules.  Note that
-- this is only meaningful for TokenCat.
terminalName :: String -> Doc
terminalName cat =
    case cat of
        "Ident"   -> "L_ident"
        "String"  -> "L_quoted"
        "Integer" -> "L_integ"
        "Double"  -> "L_doubl"
        "Char"    -> "L_charac"
        own       -> "L_" <> text own


-- | Useful to be able to use a literal string directly as an identifier in
-- an Happy AST.
-- E.g. instead of writing
-- Happy.EApp (Happy.EIdent (Happy.Ident "foo")) (Happy.EIdent (Happy.Ident "bar"")
-- We can write:
-- Happy.EApp "foo" "bar"
instance IsString Happy.Expression where
    fromString = Happy.EIdent . Happy.Ident
