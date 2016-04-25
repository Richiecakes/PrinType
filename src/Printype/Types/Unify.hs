{-|
Module      : Printype.Types.Unify
Description : Implements Robinson's type unification algorithm.
Copyright   : Richard Appleby 2016

A unifier U of two types A, B is a substitution S such that S(A) = S(B).
A most general unifier or mgu U of two types A and B is a unifier such that every other
unifier factors through U.
-}

module Printype.Types.Unify(mgu, mgu_sequence) where

import Printype.Types.Syntax
import qualified Printype.Types.Substitutions as Sub

-- Two mismatching types found in two larger types.
-- DTI: The left type is atomic.
type Mismatch = (Type, Type)

-- Given two types, determine whether they are the same,
-- and if they are not, determine the leftmost position
-- at which they differ.
find_mismatch :: Type -> Type -> Maybe Mismatch
find_mismatch (TVar x) (TVar y) = if x == y then Nothing else Just ((TVar x), (TVar y))
find_mismatch (TVar x) t = Just ((TVar x), t)
find_mismatch t (TVar y) = Just ((TVar y), t)
find_mismatch (t1 :=> t2) (t1' :=> t2') =
  case find_mismatch t1 t1' of
    Nothing -> find_mismatch t2 t2'
    Just (m1, m2) -> Just (m1, m2)

-- Returns the most general unifier of two types, should one exist.
mgu :: Type -> Type -> Maybe Sub.Substitution
mgu t1 t2 = mgu' Sub.empty t1 t2

mgu' :: Sub.Substitution -> Type -> Type -> Maybe Sub.Substitution
mgu' u t1 t2 =
  case find_mismatch (Sub.apply u t1) (Sub.apply u t2) of
    Nothing -> Just u
    Just ((TVar c), c') ->
      if c `contains_tv` c' then Nothing
      else mgu' ((Sub.singleton c c') `Sub.compose` u) t1 t2

-- Returns the most general unifier of two sequences of types, should one exist.
mgu_sequence :: [Type] -> [Type] -> Maybe Sub.Substitution
mgu_sequence t1s t2s =
  let tv = fresh_tv' (t1s ++ t2s)
      t1 = arrowR (t1s ++ [(TVar tv)])
      t2 = arrowR (t2s ++ [(TVar tv)])
      f = (\u -> u `Sub.remove` tv)
      in fmap f $ mgu t1 t2     

-- L => A => M => B => D => A
sampleLambda :: Type
sampleLambda = arrowR' [1,2,3,4,5,2]

-- C => A => L => C => U => L => U => S
sampleCalculus :: Type
sampleCalculus = arrowR' [6,2,1,6,7,1,7,8]