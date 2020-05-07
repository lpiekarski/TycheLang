module RunGrammar where

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad (when)

import LexGrammar
import ParGrammar
import SkelGrammar
import PrintGrammar
import AbsGrammar

import ErrM

runProgram :: Program (Maybe (Int, Int)) -> IO ()
runProgram prog = case transProgram prog of
  Ok str -> putStr str
  Bad str -> do 
               putStr "Error: "
               putStr str
               putStrLn ""