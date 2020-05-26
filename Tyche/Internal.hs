module Tyche.Internal where

import           Tyche.Abs
import           Tyche.Types

type InternalDef = (Ident, FullType LineInfo, Val)

internals :: [InternalDef]
internals =
  [ internal_zero
  , internal_print
  , internal_exit
  ]

internal_zero =
  ( Ident "zero"
  , readonlyIntT
  , IntVal 0
  )

internal_print =
  ( Ident "print"
  , readonlyFunctionT [varArgT readonlyStringT] voidT
  , FuncVal (\venv -> \lenv -> \icont -> \(store, input) -> do
    let (outerr, output) = icont venv (store, input)
    (outerr, ("xd" ++ output))
  ))

internal_exit =
  ( Ident "exit"
  , readonlyFunctionT [] voidT
  , FuncVal (\venv -> \lenv -> \icont -> \(store, input) -> (NoErr, ""))
  )
