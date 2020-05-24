module Tyche.RunProgram where

import           Control.Monad      (when)
import           System.Environment (getArgs, getProgName)
import           System.Exit        (exitFailure, exitSuccess)
import           System.IO          (hGetContents, stdin)

import           Tyche.Abs
import           Tyche.Lex
import           Tyche.Par
import           Tyche.Print
import           Tyche.Trans
import           Tyche.TypeCheck
import           Tyche.Types

import           Tyche.ErrM

runProgram :: Program LineInfo -> IO ()
runProgram prog = do
  case typecheckProgram prog of
    Ok _ -> do
      tp <- transProgram prog
      case tp of
        Ok () -> return ()
        Bad str -> do
          putStr "Runtime Error:\n"
          putStr str
          return ()
    Bad str -> do
      putStr "Static Error:\n"
      putStr str
      return ()
