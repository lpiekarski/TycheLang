module Tyche.Bool where

import           Tyche.Types

negateBool :: Val -> Val
negateBool (BoolVal v) = BoolVal (not v)
negateBool v           = v
