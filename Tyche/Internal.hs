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
  , FuncVal (\venv -> \lenv -> \icont -> \(store, stacktrace, input) -> do
    let (outerr, outstacktrace, output) = icont venv (store, stacktrace, input)
    (outerr, outstacktrace, ("xd" ++ output))
  ))

internal_exit =
  ( Ident "exit"
  , readonlyFunctionT [] voidT
  , FuncVal (\venv -> \lenv -> \icont -> \(store, stacktrace, input) -> (NoErr, stacktrace, ""))
  )
