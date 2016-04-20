-------------------------------------------------------------
-- PRINCIPAL
-- Module implementing the principal type deduction algorithm
-- for finding most general types of lambda terms.
-- Type contexts.
-- Deductions.
-------------------------------------------------------------

module Printype.Principal.Contexts(
  Context(..),
  cmap,
  empty_context,
  to_list,
  type_in_context,
  common_vars,
  (\\\),
  context_union) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (intersperse)

import Printype.Lambda
import Printype.Types

-- TYPE CONTEXTS --

-- A context under which a particular term is typed.
-- eg. {x:A, y:A => B}
newtype Context = Ctx (Map.Map Char Type)

instance Show Context where
  show (Ctx m) =
    let kvs = Map.toAscList m
        kvstrings = map (\ (k, v) -> [k] ++ ":" ++ show v) kvs
        in "{" ++ concat (intersperse ", " kvstrings) ++ "}"

instance Substitutible Context where
  applysub c = cmap (applysub c)
  tvars (Ctx m) = Set.unions $ Map.elems $ fmap tvars m

cmap :: (Type -> Type) -> Context -> Context
cmap f (Ctx m) = Ctx $ Map.map f m

-- The empty type context {}.
empty_context :: Context
empty_context = Ctx $ Map.empty

to_list :: Context -> [(Char, Type)]
to_list (Ctx m) = Map.toAscList m

-- Determine the type given to a particular variable under the type context.
type_in_context :: Context -> Char -> Maybe Type
type_in_context (Ctx m) x = Map.lookup x m

-- Returns a list containing the variables referenced in both contexts.
common_vars :: Context -> Context -> [Char]
common_vars (Ctx m1) (Ctx m2) = Set.toAscList $ Set.intersection (Map.keysSet m1) (Map.keysSet m2)

-- Removes variable x and associated binding from a type context.
(\\\) :: Context -> Char -> Context
(Ctx m) \\\ x = Ctx $ Map.delete x m

-- Left biased union of two contexts.
context_union :: Context -> Context -> Context
context_union (Ctx c1) (Ctx c2) = Ctx $ Map.union c1 c2
