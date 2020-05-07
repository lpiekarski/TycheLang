module Tyche.RunProgram where

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad (when)

import Tyche.Lex
import Tyche.Par
import Tyche.Trans
import Tyche.Print
import Tyche.Abs

import Tyche.ErrM

runProgram :: Program (Maybe (Int, Int)) -> IO ()
runProgram prog = case transProgram prog of
  Ok str -> putStr str
  Bad str -> do 
               putStr "Error: "
               putStr str
               putStrLn ""