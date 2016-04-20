{-|
Module      : Printype.Types.Substitutions
Description : Provides an implementation of type substitutions using maps.
Copyright   : Richard Appleby 2016

A type substitution is a map from type variables to types.
A type substitution can be applied to any member of the Substitutible typeclass.
-}

module Printype.Types.Substitutions(
  Substitution, Substitutible(applysub, tvars),
  empty_sub, unsubstitute, compose, singleton) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Printype.Types.Syntax

-- SUBSTITUTIONS --

-- Datatype for substitutions of the form [C_1/c_1, ..., C_n/c_n]
-- for type variables c_i and C_i ranging over types.

newtype Substitution = Sub (Map.Map Int Type) deriving Show

-- Datatypes which can have type substitutions applied to.

class Substitutible a where
  applysub :: Substitution -> a -> a
  tvars :: a -> Set.Set Int

instance Substitutible Type where
  applysub (Sub s) = foldType fv fa
    where fv = (\ x -> Map.findWithDefault (TVar x) x s)
          fa = (:=>)
  tvars = foldType (Set.singleton) (Set.union)

-- The empty substitution [].
empty_sub :: Substitution
empty_sub = Sub $ Map.empty

-- Remove the term [X/x] from the given substitution, if one exists.
unsubstitute :: Substitution -> Int -> Substitution
unsubstitute (Sub s) x = Sub $ Map.delete x s

-- Sequential composition of two type substitutions.
-- (s.t)(A) = s(t(A))
compose :: Substitution -> Substitution -> Substitution
compose (Sub s) (Sub t) =
  let st = Map.map (\ tv -> applysub (Sub s) tv) t
  in Sub $ Map.unionWith seq s st

-- The singleton substitution [t/x].
singleton :: Int -> Type -> Substitution
singleton x t = Sub $ Map.singleton x t 