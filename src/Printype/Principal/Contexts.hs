{-|
Module      : Printype.Principal.Contexts
Description : Type contexts (environments) used in type deductions.
Copyright   : Richard Appleby 2016

A type context provides an environment for a term to be typed in.
A type context can be implemented as a map from term variables to types.
-}

module Printype.Principal.Contexts(
  -- $contexts

  -- * Datatype
  Context,
  -- * Construction
  empty,
  singleton,
  -- * Update
  union,
  remove,
  -- * Query
  lookup,
  commonSubjects,
  -- * Conversion
  toList) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (intersperse)

import Prelude hiding (lookup)

import Printype.Lambda
import Printype.Types
import Printype.Types.Substitutions hiding (empty, singleton, remove)

-- $contexts
-- Standard implementation of type contexts (environments) using Map.
-- This module is intended to be imported qualified, e.g.
--
-- > import qualified Printype.Principal.Contexts as Ctx

-- |A context under which a particular term is typed.
-- eg. {x:A, y:A => B}
newtype Context = Ctx (Map.Map Char Type)

instance Show Context where
  show (Ctx m) =
    let kvs = Map.toAscList m
        kvstrings = map (\ (k, v) -> [k] ++ ":" ++ show v) kvs
        in "{" ++ concat (intersperse ", " kvstrings) ++ "}"

instance Substitutable Context where
  apply c = cmap (apply c)
  subjects (Ctx m) = Set.unions $ Map.elems $ fmap subjects m

-- |The empty type context {}.
empty :: Context
empty = Ctx $ Map.empty

-- |A singleton type context of the form {x:A}.
singleton :: Char -> Type -> Context
singleton x t = Ctx $ Map.singleton x t

-- |View the context as a list of pairs.
toList :: Context -> [(Char, Type)]
toList (Ctx m) = Map.toAscList m

-- |Determine the type given to a particular variable in the type context.
lookup :: Context -> Char -> Maybe Type
lookup (Ctx m) x = Map.lookup x m

-- |Returns a list containing the variables referenced in both contexts.
commonSubjects :: Context -> Context -> [Char]
commonSubjects (Ctx m1) (Ctx m2) = Set.toAscList $ Set.intersection (Map.keysSet m1) (Map.keysSet m2)

-- |Removes variable x and associated binding from a type context.
remove :: Context -> Char -> Context
remove (Ctx m) x = Ctx $ Map.delete x m

-- |Left biased union of two contexts.
union :: Context -> Context -> Context
union (Ctx c1) (Ctx c2) = Ctx $ Map.union c1 c2

cmap :: (Type -> Type) -> Context -> Context
cmap f (Ctx m) = Ctx $ Map.map f m
