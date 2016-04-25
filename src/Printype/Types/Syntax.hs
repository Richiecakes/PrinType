{-|
Module      : Printype.Types.Syntax
Description : Provides a syntax for basic types.
Copyright   : Richard Appleby 2016

A basic type is either a type variable or an arrow type of the form t1 :=> t2.
The module also provides several utility functions for constructing types.
-}

module Printype.Types.Syntax(
	Type(..), foldType,
	arrowR, arrowL, arrowR', arrowL',
	contains_tv, is_arrow_type,
	argtype, restype,
	fresh_tv, fresh_tv') where

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