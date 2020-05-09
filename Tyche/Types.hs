module Tyche.Types where

import Tyche.ErrM
import Tyche.Abs

extendFunc :: (a -> b) -> a -> b -> a -> b
extendFunc f nx ny x = if nx == x then ny else f x

type Result = Err String
type Loc = Int
type Val = Int
type Var = String
type State = Loc -> Val
type Cont = State -> State
type VEnv = Var -> Loc
type ICont = VEnv -> Cont

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

type TypeCheckResult = Err (Maybe (FullType (Maybe (Int, Int))))
type TEnv = Var -> Maybe (FullType (Maybe (Int, Int)))

typecheckfailure :: Show a => a -> TypeCheckResult
typecheckfailure x = Bad $ "Udefined case for type check: " ++ show x
