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
  , internal_print_string
  , internal_print_int
  , internal_print_float
  , internal_println
  , internal_exit
  , internal_get_char
  , internal_add_char
  , internal_string_length
  , internal_random_int
  , internal_random_float
  ]

internal_read_char =
  ( Ident "read_char"
  , readonlyFunctionT [] intT
  , FuncVal [] (\args -> \venv -> \lenv -> \icont -> \(store, input@(ch:istream, randstream)) -> do
    case lenv LReturn of
      Nothing          -> errMsg "internal error (return label is Nothing)" (store, input)
      Just returnecont -> returnecont (IntVal (toInteger $ ord ch)) (store, (istream, randstream))
  ))

internal_print_char =
  ( Ident "print_char"
  , readonlyFunctionT [valArgT readonlyIntT] voidT
  , FuncVal [argT valT (identT "ch") readonlyIntT] (\[Value (IntVal char) ident] -> \venv -> \lenv -> \icont -> \(store, input) -> do
    let (outerr, output) = icont venv (store, input)
    (outerr, ((chr (fromIntegral char)):output))
  ))

internal_print_string =
  ( Ident "print_string"
  , readonlyFunctionT [valArgT readonlyStringT] voidT
  , FuncVal [argT valT (identT "str") readonlyStringT] (\[Value (StringVal str) ident] -> \venv -> \lenv -> \icont -> \(store, input) -> do
    let (outerr, output) = icont venv (store, input)
    (outerr, (str ++ output))
  ))

internal_print_int =
  ( Ident "print_int"
  , readonlyFunctionT [valArgT readonlyIntT] voidT
  , FuncVal [argT valT (identT "num") readonlyIntT] (\[Value (IntVal int) ident] -> \venv -> \lenv -> \icont -> \(store, input) -> do
    let (outerr, output) = icont venv (store, input)
    (outerr, ((show int) ++ output))
  ))

internal_print_float =
  ( Ident "print_float"
  , readonlyFunctionT [valArgT readonlyFloatT] voidT
  , FuncVal [argT valT (identT "flt") readonlyFloatT] (\[Value (FloatVal flt) ident] -> \venv -> \lenv -> \icont -> \(store, input) -> do
    let (outerr, output) = icont venv (store, input)
    (outerr, ((show flt) ++ output))
  ))

internal_println =
  ( Ident "println"
  , readonlyFunctionT [] voidT
  , FuncVal [] (\[] -> \venv -> \lenv -> \icont -> \(store, input) -> do
    let (outerr, output) = icont venv (store, input)
    (outerr, ("\n" ++ output))
  ))

internal_exit =
  ( Ident "exit"
  , readonlyFunctionT [] voidT
  , FuncVal [] (\args -> \venv -> \lenv -> \icont -> \(store, input) -> (NoErr, ""))
  )

internal_get_char =
  ( Ident "get_char"
  , readonlyFunctionT [valArgT readonlyStringT, valArgT readonlyIntT] intT
  , FuncVal [argT valT (identT "str") readonlyStringT, argT valT (identT "id") readonlyIntT] (\[Value (StringVal str) identstr, Value (IntVal id) identid] -> \venv -> \lenv -> \icont -> do
    case lenv LReturn of
      Nothing          -> errMsg "internal error (return label is Nothing)"
      Just returnecont -> returnecont (IntVal (toInteger $ ord (str!!(fromIntegral id))))
  ))

internal_add_char =
  ( Ident "add_char"
  , readonlyFunctionT [varArgT stringT, varArgT readonlyIntT] voidT
  , FuncVal [argT varT (identT "str") stringT, argT varT (identT "ch") readonlyIntT] (\args -> \venv -> \lenv -> \icont -> \(store, input) -> do
    let (outerr, output) = icont venv (store, input)
    (outerr, ("NOT IMPLEMENTED\n" ++ output))
  ))

internal_string_length =
  ( Ident "string_length"
  , readonlyFunctionT [valArgT stringT] intT
  , FuncVal [argT valT (identT "str") stringT] (\[Value (StringVal str) ident] -> \venv -> \lenv -> \icont -> do
    case lenv LReturn of
      Nothing          -> errMsg "internal error (return label is Nothing)"
      Just returnecont -> returnecont (IntVal (toInteger $ length str))
  ))

internal_random_int =
  ( Ident "random_int"
  , readonlyFunctionT [] intT
  , FuncVal [] (\args -> \venv -> \lenv -> \icont -> \(store, input@(istream, randint:randstream)) -> do
    case lenv LReturn of
      Nothing          -> errMsg "internal error (return label is Nothing)" (store, input)
      Just returnecont -> returnecont (IntVal (toInteger randint)) (store, (istream, randstream))
  ))

internal_random_float =
  ( Ident "random_float"
  , readonlyFunctionT [] floatT
  , FuncVal [] (\args -> \venv -> \lenv -> \icont -> \(store, input@(istream, randint:randstream)) -> do
    case lenv LReturn of
      Nothing          -> errMsg "internal error (return label is Nothing)" (store, input)
      Just returnecont -> returnecont (FloatVal ((1.0 + ((fromIntegral randint) / (fromIntegral (maxBound::Int)))) / 2.0)) (store, (istream, randstream))
  ))
