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
  , FuncVal (\lenv -> \icont -> \(store, stacktrace, input) ->
    (NoErr, stacktrace, (stringToOutput "xd" EOO{-(icont )-}))
  ))

internal_exit =
  ( Ident "exit"
  , readonlyFunctionT [] voidT
  , FuncVal (\lenv -> \icont -> \(store, stacktrace, input) -> (NoErr, stacktrace, EOO))
  )
