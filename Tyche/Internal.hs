module Tyche.Internal where

import           Tyche.Abs
import           Tyche.Interpreter
import           Tyche.Trans
import           Tyche.Types

import           Data.Char

type InternalDef = (Ident, FullType LineInfo, Val)

internals :: [InternalDef]
internals =
  [ internal_read_char
  , internal_print_char
  , internal_exit
  , internal_get_char
  , internal_add_char
  ]

internal_read_char =
  ( Ident "read_char"
  , readonlyFunctionT [] intT
  , FuncVal [] (\args -> \venv -> \lenv -> \icont -> \(store, input) -> do
    let (outerr, output) = icont venv (store, input)
    (outerr, ("NOT IMPLEMENTED\n" ++ output))
  ))

internal_print_char =
  ( Ident "print_char"
  , readonlyFunctionT [varArgT readonlyIntT] voidT
  , FuncVal [valArgT readonlyIntT] (\[Value (IntVal char) ident] -> \venv -> \lenv -> \icont -> \(store, input) -> do
    let (outerr, output) = icont venv (store, input)
    (outerr, ((chr (fromIntegral char)):output))
  ))

internal_exit =
  ( Ident "exit"
  , readonlyFunctionT [] voidT
  , FuncVal [] (\args -> \venv -> \lenv -> \icont -> \(store, input) -> (NoErr, ""))
  )

internal_get_char =
  ( Ident "get_char"
  , readonlyFunctionT [varArgT readonlyStringT, varArgT readonlyIntT] intT
  , FuncVal [varArgT readonlyStringT, varArgT readonlyIntT] (\args -> \venv -> \lenv -> \icont -> \(store, input) -> do
    let (outerr, output) = icont venv (store, input)
    (outerr, ("NOT IMPLEMENTED\n" ++ output))
  ))

internal_add_char =
  ( Ident "add_char"
  , readonlyFunctionT [varArgT stringT] voidT
  , FuncVal [varArgT stringT] (\args -> \venv -> \lenv -> \icont -> \(store, input) -> do
    let (outerr, output) = icont venv (store, input)
    (outerr, ("NOT IMPLEMENTED\n" ++ output))
  ))
