module Tyche.RunProgram where

import           Control.Monad      (when)
import           System.Environment (getArgs, getProgName)
import           System.Exit        (exitFailure, exitSuccess)
import           System.IO          (hGetContents, stdin)
import           System.Random

import           Tyche.Abs
import           Tyche.ErrM
import           Tyche.Layout
import           Tyche.Lex
import           Tyche.Par
import           Tyche.Print
import           Tyche.State
import           Tyche.TransProgram
import           Tyche.TypeCheck
import           Tyche.Types

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
    , "  (files)         Parse content of files."
    ]
  exitFailure

runProgram :: Program LineInfo -> IO ()
runProgram prog = do
  case typecheckProgram prog of
    Ok _ -> do
        let randomStream = randoms (mkStdGen 42) :: RandomStream
        interact (\inputstr ->
          let
            (err, output) = transProgram prog (inputstr, randomStream)
          in
            output ++ (case err of
              NoErr      -> ""
              ErrMsg str -> "\nRuntime Error: " ++ str))
    Bad str -> do
      putStr "\nStatic Error:\n"
      putStr str
      return ()
