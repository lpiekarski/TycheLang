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

runTree :: (Show a, Print a) => a -> IO ()
runTree tree
 = do
      putStrLn "asd"