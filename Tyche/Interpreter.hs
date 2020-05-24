module Main where


import           Control.Monad      (when)
import           System.Environment (getArgs, getProgName)
import           System.Exit        (exitFailure, exitSuccess)
import           System.IO          (hGetContents, stdin)

import           Tyche.Abs
import           Tyche.Layout
import           Tyche.Lex
import           Tyche.Par
import           Tyche.Print
import           Tyche.RunProgram
import           Tyche.Trans

import           Tyche.ErrM


lexer = resolveLayout True . myLexer

runFile :: FilePath -> IO ()
runFile f = readFile f >>= run

run :: String -> IO ()
run s = let ts = lexer s in case pProgram ts of
           Bad s    -> do putStrLn s
                          exitFailure
           Ok  prog -> do runProgram prog
                          exitSuccess

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse content from stdin."
    , "  (files)         Parse content of files."
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    []         -> getContents >>= run
    fs         -> mapM_ runFile fs
