module Main where


import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad (when)

import LexGrammar
import ParGrammar
import SkelGrammar
import PrintGrammar
import AbsGrammar
import RunGrammar




import ErrM

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

runFile :: (Print a, Show a) => ParseFun a -> FilePath -> IO ()
runFile p f = readFile f >>= run p

run :: (Print a, Show a) => ParseFun a -> String -> IO ()
run p s = let ts = myLLexer s in case p ts of
           Bad s    -> do putStrLn s
                          exitFailure
           Ok  tree -> do runTree tree
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
    [] -> getContents >>= run pProgram
    fs -> mapM_ (runFile pProgram) fs





