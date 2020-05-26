module Tyche.TransProgram where

import           Tyche.Abs
import           Tyche.Env
import           Tyche.Internal
import           Tyche.State
import           Tyche.Trans
import           Tyche.Types

transProgram :: Program LineInfo -> Input -> Ans
transProgram x input = case x of
  Program lineinfo stmts -> do
    let defaultvenv = \ident -> Nothing
    let defaultlenv = \label -> Nothing
    let defaultstore = (0, \loc -> NoVal)
    let defaultans = (NoErr, "")
    let defaulticont =  (\venv -> (\state -> defaultans))
    let
      addInternals :: [(Ident, FullType LineInfo, Val)] -> VEnv -> Store -> (VEnv, Store)
      addInternals [] venv store = (venv, store)
      addInternals ((ident, _, val):ints) venv store = do
          let (varloc, storeaftervardef) = newLoc store
          let venvaftervardef = extendFunc venv ident (Just varloc)
          let storeaftervalsave = saveInStore storeaftervardef varloc val
          addInternals ints venvaftervardef storeaftervalsave
    let (venvwithinternals, storewithinternals) = addInternals internals defaultvenv defaultstore
    let defaultstate = (storewithinternals, input)
    let cont = transStmts stmts venvwithinternals defaultlenv defaulticont
    cont defaultstate
