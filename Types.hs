-------------------------------------------------------------
-- TYPES
-- Simple types. Type constructors and predicates.
-- Type substitutions.
-------------------------------------------------------------

module Types(
	Type(..),
	arrowR, arrowL, arrowR', arrowL',
	contains_tv, is_arrow_type,
	argtype, restype,
	fresh_tv, fresh_tv',
	Substitution, Substitutible(applysub, tvars),
	empty_sub, unsubstitute, compose, singleton) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- TYPES --

-- Standard data type representing types using ints as type variables.
data Type = TVar Int | Type :=> Type

instance Show Type where
	show (TVar a) = show a
	show ((l1 :=> l2) :=> r) = "(" ++ show l1 ++ " => " ++ show l2 ++ ") => " ++ show r
	show (l :=> (r1 :=> r2)) = show l ++ " => " ++ show r1 ++ " => " ++ show r2
	show (t1 :=> t2) = show t1  ++ " => " ++ show t2

foldType :: (Int -> b) -> (b -> b -> b) -> Type -> b
foldType fv fa (TVar x) = fv x
foldType fv fa (t1 :=> t2) = (foldType fv fa t1) `fa` (foldType fv fa t2)

-- Connect multiple types by arrows, associating to the right.
arrowR :: [Type] -> Type
arrowR = foldr1 (:=>)

-- Connect multiple types by arrows, associating to the left.
arrowL :: [Type] -> Type
arrowL = foldl1 (:=>)

-- Connect multiple type variables by arrows, associating to the right.
arrowR' :: [Int] -> Type
arrowR' = arrowR . (map TVar)

-- Connect multiple type variables by arrows, associating to the left.
arrowL' :: [Int] -> Type
arrowL' = arrowL . (map TVar)

-- Does type t contain type variable x?
contains_tv :: Int -> Type -> Bool
contains_tv x = foldType ((==) x) (||)

-- Is type t of the form A :=> B?
is_arrow_type :: Type -> Bool
is_arrow_type (_ :=> _) = True
is_arrow_type _ = False

-- Get the argument type of some arrow type.
argtype :: Type -> Type
argtype (t1 :=> t2) = t1
argtype _ = error "argtype can only be applied to arrow types"

-- Get the result type of some arrow type.
restype :: Type -> Type
restype (t1 :=> t2) = t2
restype _ = error "restype can only be applied to arrow types"

-- Finds a type variable which is not present in the given type.
fresh_tv :: Type -> Int
fresh_tv = (+1) . foldType id max

-- Finds a type variable which is not present in the given types.
fresh_tv' :: [Type] -> Int
fresh_tv' = ((+1) . foldl1 max . map fresh_tv)
      

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