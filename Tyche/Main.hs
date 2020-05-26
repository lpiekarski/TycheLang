module Main where

import           Control.Monad      (when)
import           System.Environment (getArgs, getProgName)
import           System.Exit        (exitFailure, exitSuccess)
import           System.IO          (hGetContents, stdin)

import           Tyche.RunProgram

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    fs         -> mapM_ runFile fs
