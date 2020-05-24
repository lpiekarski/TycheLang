module Tyche.RunProgram where

import           Control.Monad      (when)
import           System.Environment (getArgs, getProgName)
import           System.Exit        (exitFailure, exitSuccess)
import           System.IO          (hGetContents, stdin)

import           Tyche.Abs
import           Tyche.Lex
import           Tyche.Par
import           Tyche.Print
--import           Tyche.Trans
import           Tyche.TypeCheck

import           Tyche.ErrM

runProgram :: Program (Maybe (Int, Int)) -> IO ()
runProgram prog = do
  putStrLn $ printTree prog
  case typecheckProgram prog of
    Ok _ -> do
      {-tp <- transProgram prog
      case tp of
        Ok () -> return ()
        Bad str -> do
          putStr "Runtime Error: "
          putStr str
          putStrLn ""
          return ()-}
      putStrLn "OK"
    Bad str -> do
      putStr "Type Error:\n"
      putStr str
      return ()
