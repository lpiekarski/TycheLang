module Tyche.RunProgram where

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad (when)

import Tyche.Lex
import Tyche.Par
import Tyche.Trans
import Tyche.TypeCheck
import Tyche.Print
import Tyche.Abs

import Tyche.ErrM

runProgram :: Program (Maybe (Int, Int)) -> IO ()
runProgram prog = do
  --case typecheckProgram prog of
  --  Ok _ -> do
      tp <- transProgram prog
      case tp of
        Ok () -> return ()
        Bad str -> do 
          putStr "Runtime Error: "
          putStr str
          putStrLn ""
          return ()
    --Bad str -> do
    --  putStr "TypeCheck Error: "
    --  putStr str
    --  putStrLn ""
    --  return ()