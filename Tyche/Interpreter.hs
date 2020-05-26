module Tyche.Interpreter where

import           Tyche.Abs
import           Tyche.Layout
import           Tyche.Lex
import           Tyche.Par
import           Tyche.Print
import           Tyche.Trans

import           Tyche.ErrM

lexer = resolveLayout True . myLexer

stringToStmts :: String -> [Stmt (Maybe (Int, Int))]
stringToStmts s = let ts = lexer s in case pListStmt ts of Ok prog -> prog
