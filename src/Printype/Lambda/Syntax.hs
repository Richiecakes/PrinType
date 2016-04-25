{-|
Module      : Printype.Lambda.Syntax
Description : Basic syntax and helper functions for terms of the lambda calculus.
Copyright   : Richard Appleby 2016

Datatype, construction functions and predicates for terms of the untyped lambda calculus.
-}

module Printype.Lambda.Syntax(
  -- * Syntax
  Term(..),
  -- * Constructor functions
  var,
  lam,
  app,
  lams,
  appL,
  appR,
  appL',
  appR',
  nfoldapp,
  -- * Predicates
  free_in) where

-- |Standard data type representing lambda terms using chars as variables.
data Term = Var Char | Lam Char Term | App Term Term

instance Show Term where
  show (App t1 (App t2 t3)) = show t1 ++ "(" ++ show t2 ++ show t3 ++ ")"
  show (App (App t1 t2) t3) = show t1 ++ show t2 ++ show t3
  show (Var x) = [x]
  show (Lam x (Lam y t)) = "\\" ++ [x,y] ++ "." ++ show t
  show (Lam x t) = "\\" ++ [x] ++ "." ++ show t
  show (App t1 t2) = show t1 ++ show t2

var :: Char -> Term
var = Var

lam :: Char -> Term -> Term
lam = Lam

-- |Abstracts multiple variables at once.
lams :: [Char] -> Term -> Term
lams [] p = p
lams (x:xs) p = lam x (lams xs p)

app :: Term -> Term -> Term
app = App

-- |Left associative repeat application.
appL :: [Term] -> Term
appL = foldl1 app

-- |Right associative repeat application.
appR :: [Term] -> Term
appR = foldr1 app

-- |Left associative repeat term variable application.
appL' :: [Char] -> Term
appL' = appL . (map Var)

-- |Right associative repeat term variable application.
appR' :: [Char] -> Term
appR' = appR . (map Var)

{-|
N-fold application, used in church numerals for example. Applies f n times to x.

> nfoldapp n f x = f(f(...f(x)...))
-}
nfoldapp :: Int -> Term -> Term -> Term
nfoldapp n f x
  | n < 0     = error "nfoldapp must take an integer >= 0"
  | otherwise = appR $ (replicate n f) ++ [x]

-- |Predicate for checking whether the given type variable occurs free in the given term.
free_in :: Char -> Term -> Bool
free_in x (Var y) = x == y
free_in x (Lam y t) = x /= y && free_in x t
free_in x (App t1 t2) = (free_in x t1) || (free_in x t2)