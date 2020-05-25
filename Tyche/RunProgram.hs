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
      interact (\inputstr -> do
        let (err, stacktrace, output) = transProgram prog (stringToInput inputstr)
        (outputToString output) ++ (case err of
          NoErr      -> ""
          ErrMsg str -> "\nRuntime Error:\n" ++ str ++ "\n"))
    Bad str -> do
      putStr "Static Error:\n"
      putStr str
      return ()
