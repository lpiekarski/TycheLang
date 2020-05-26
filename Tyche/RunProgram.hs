module Tyche.RunProgram where

import           Control.Monad      (when)
import           System.Environment (getArgs, getProgName)
import           System.Exit        (exitFailure, exitSuccess)
import           System.IO          (hGetContents, stdin)

import           Tyche.Abs
import           Tyche.ErrM
import           Tyche.Lex
import           Tyche.Par
import           Tyche.Print
import           Tyche.State
import           Tyche.Trans
import           Tyche.TypeCheck
import           Tyche.Types

runProgram :: Program LineInfo -> IO ()
runProgram prog = do
  case typecheckProgram prog of
    Ok _ -> do
        interact (\inputstr ->
          let
            (err, output) = transProgram prog inputstr
          in
            output ++ (case err of
              NoErr      -> ""
              ErrMsg str -> "\nRuntime Error: " ++ str))
    Bad str -> do
      putStr "\nStatic Error:\n"
      putStr str
      return ()
