module Tyche.Internal where

import           Tyche.Abs
import           Tyche.Types

type InternalDef = (Ident, FullType LineInfo, Val)

internals :: [InternalDef]
internals =
  [ zero
  ]

zero :: InternalDef
zero = (Ident "zero", intT, IntVal 0)
