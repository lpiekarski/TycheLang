module Tyche.Numerical where

import           Tyche.Abs
import           Tyche.Types

import           Data.Fixed

unifyNumericTypes :: FullType LineInfo -> FullType LineInfo -> FullType LineInfo
unifyNumericTypes ft1 ft2 =
  if isFloat ft1 || isFloat ft2 then
    floatT
  else
    intT

negateNumerical :: Val -> Val
negateNumerical (IntVal v)   = IntVal (-v)
negateNumerical (FloatVal v) = FloatVal (-v)
negateNumerical v            = v

addNumericals :: Val -> Val -> Val
addNumericals v1 v2 = case (v1, v2) of
  (IntVal x1, IntVal x2)     -> IntVal (x1 + x2)
  (IntVal x1, FloatVal x2)   -> FloatVal ((fromIntegral x1) + x2)
  (FloatVal x1, IntVal x2)   -> FloatVal (x1 + (fromIntegral x2))
  (FloatVal x1, FloatVal x2) -> FloatVal (x1 + x2)

substractNumericals :: Val -> Val -> Val
substractNumericals v1 v2 = case (v1, v2) of
  (IntVal x1, IntVal x2)     -> IntVal (x1 - x2)
  (IntVal x1, FloatVal x2)   -> FloatVal ((fromIntegral x1) - x2)
  (FloatVal x1, IntVal x2)   -> FloatVal (x1 - (fromIntegral x2))
  (FloatVal x1, FloatVal x2) -> FloatVal (x1 - x2)

multiplyNumericals :: Val -> Val -> Val
multiplyNumericals v1 v2 = case (v1, v2) of
  (IntVal x1, IntVal x2)     -> IntVal (x1 * x2)
  (IntVal x1, FloatVal x2)   -> FloatVal ((fromIntegral x1) * x2)
  (FloatVal x1, IntVal x2)   -> FloatVal (x1 * (fromIntegral x2))
  (FloatVal x1, FloatVal x2) -> FloatVal (x1 * x2)

divideNumericals :: Val -> Val -> Val
divideNumericals v1 v2 = case (v1, v2) of
  (IntVal x1, IntVal x2)     -> IntVal (x1 `div` x2)
  (IntVal x1, FloatVal x2)   -> FloatVal ((fromIntegral x1) / x2)
  (FloatVal x1, IntVal x2)   -> FloatVal (x1 / (fromIntegral x2))
  (FloatVal x1, FloatVal x2) -> FloatVal (x1 / x2)


modNumericals :: Val -> Val -> Val
modNumericals v1 v2 = case (v1, v2) of
  (IntVal x1, IntVal x2)     -> IntVal (x1 `mod` x2)
  (IntVal x1, FloatVal x2)   -> FloatVal ((fromIntegral x1) `mod'` x2)
  (FloatVal x1, IntVal x2)   -> FloatVal (x1 `mod'` (fromIntegral x2))
  (FloatVal x1, FloatVal x2) -> FloatVal (x1 `mod'` x2)

neNumericals :: Val -> Val -> Val
neNumericals v1 v2 = case (v1, v2) of
  (IntVal x1, IntVal x2)     -> BoolVal (x1 /= x2)
  (IntVal x1, FloatVal x2)   -> BoolVal ((fromIntegral x1) /= x2)
  (FloatVal x1, IntVal x2)   -> BoolVal (x1 /= (fromIntegral x2))
  (FloatVal x1, FloatVal x2) -> BoolVal (x1 /= x2)

equNumericals :: Val -> Val -> Val
equNumericals v1 v2 = case (v1, v2) of
  (IntVal x1, IntVal x2)     -> BoolVal (x1 == x2)
  (IntVal x1, FloatVal x2)   -> BoolVal ((fromIntegral x1) == x2)
  (FloatVal x1, IntVal x2)   -> BoolVal (x1 == (fromIntegral x2))
  (FloatVal x1, FloatVal x2) -> BoolVal (x1 == x2)

geNumericals :: Val -> Val -> Val
geNumericals v1 v2 = case (v1, v2) of
  (IntVal x1, IntVal x2)     -> BoolVal (x1 >= x2)
  (IntVal x1, FloatVal x2)   -> BoolVal ((fromIntegral x1) >= x2)
  (FloatVal x1, IntVal x2)   -> BoolVal (x1 >= (fromIntegral x2))
  (FloatVal x1, FloatVal x2) -> BoolVal (x1 >= x2)

leNumericals :: Val -> Val -> Val
leNumericals v1 v2 = case (v1, v2) of
  (IntVal x1, IntVal x2)     -> BoolVal (x1 <= x2)
  (IntVal x1, FloatVal x2)   -> BoolVal ((fromIntegral x1) <= x2)
  (FloatVal x1, IntVal x2)   -> BoolVal (x1 <= (fromIntegral x2))
  (FloatVal x1, FloatVal x2) -> BoolVal (x1 <= x2)

gthNumericals :: Val -> Val -> Val
gthNumericals v1 v2 = case (v1, v2) of
  (IntVal x1, IntVal x2)     -> BoolVal (x1 > x2)
  (IntVal x1, FloatVal x2)   -> BoolVal ((fromIntegral x1) > x2)
  (FloatVal x1, IntVal x2)   -> BoolVal (x1 > (fromIntegral x2))
  (FloatVal x1, FloatVal x2) -> BoolVal (x1 > x2)

lthNumericals :: Val -> Val -> Val
lthNumericals v1 v2 = case (v1, v2) of
  (IntVal x1, IntVal x2)     -> BoolVal (x1 < x2)
  (IntVal x1, FloatVal x2)   -> BoolVal ((fromIntegral x1) < x2)
  (FloatVal x1, IntVal x2)   -> BoolVal (x1 < (fromIntegral x2))
  (FloatVal x1, FloatVal x2) -> BoolVal (x1 < x2)
