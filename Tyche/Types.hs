module Tyche.Types where

import Tyche.ErrM
import Tyche.Abs

extendFunc :: (Eq a) => (a -> b) -> a -> b -> a -> b
extendFunc f nx ny x = if nx == x then ny else f x

lineInfoString :: Maybe (Int, Int) -> String
lineInfoString Nothing = ""
lineInfoString (Just (x, y)) = show x ++ ":" ++ show y

matchArgTypes :: [ArgType a] -> [ArgType a] -> Bool
matchArgTypes [] [] = True
matchArgTypes (at1:ats1) [] = False
matchArgTypes [] (at2:ats2) = False
matchArgTypes (at1:ats1) (at2:ats2) = case (at1, at2) of
  (ArgType _ _ t1, ArgType _ _ t2) -> (matchType t1 t2) && (matchArgTypes ats1 ats2)


matchType :: Type a -> Type a -> Bool
matchType t1 t2 = case (t1, t2) of
  (Int _, Int _) -> True
  (Float _, Float _) -> True
  (Str _, Str _) -> True
  (Bool _, Bool _) -> True
  (Void _, Void _) -> True
  (List _ ft1, List _ ft2) -> matchFullType ft1 ft2
  (Array _ ft1, Array _ ft2) -> matchFullType ft1 ft2
  (Fun _ ats1 ft1, Fun _ ats2 ft2) -> (matchArgTypes ats1 ats2) && (matchFullType ft1 ft2)
  otherwise -> False

matchFullType :: FullType a -> FullType a -> Bool
matchFullType ft1 ft2 = case (ft1, ft2) of
  (FullType _ _ t1, FullType _ _ t2) -> matchType t1 t2
  otherwise -> False

type Result = Err String
type Loc = Int
type Val = Int
type Var = FullIdent (Maybe (Int, Int))
type State = Loc -> Val
type Cont = State -> State
type VEnv = Var -> Loc
type ICont = VEnv -> Cont

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

type TypeCheckResult = Err (TEnv, Maybe (FullType (Maybe (Int, Int))))
type TEnv = Var -> Maybe (FullType (Maybe (Int, Int)))

typecheckfailure :: Show a => a -> TypeCheckResult
typecheckfailure x = Bad $ "Udefined case for type check: " ++ show x
