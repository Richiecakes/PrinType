{-|
Module      : Printype.Principal.Deductions
Description : Implements a syntax for type deduction trees.
Copyright   : Richard Appleby 2016

A deduction tree is a formal proof tree describing how a particular type assignment is derived.
-}

module Printype.Principal.Deductions(
  Deduction(..),
  Premise(..),
  var_deduction) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Printype.Lambda
import Printype.Types
import Printype.Principal.Contexts

-- DEDUCTIONS --

data Deduction = Ded { d_premise :: Premise
                     , d_context :: Context
                     , d_subject :: Term
                     , d_type    :: Type
                     }

data Premise = Axiom | Unary Deduction | Binary Deduction Deduction

instance Substitutible Deduction where
  applysub c (Ded Axiom ctx t a) = Ded Axiom (applysub c ctx) t (applysub c a)
  applysub c (Ded (Unary d1) ctx t a) = Ded (Unary (applysub c d1)) (applysub c ctx) t (applysub c a)
  applysub c (Ded (Binary d1 d2) ctx t a) = Ded (Binary (applysub c d1) (applysub c d2)) (applysub c ctx) t (applysub c a)

  tvars (Ded Axiom ctx t a) = Set.union (tvars ctx) (tvars a)
  tvars (Ded (Unary d1) ctx t a) = Set.unions [tvars d1, tvars ctx, tvars a]
  tvars (Ded (Binary d1 d2) ctx t a) = Set.unions [tvars d1, tvars d2, tvars ctx, tvars a]

instance Show Deduction where
  show (Ded Axiom ctx t a) = "Axiomatic deduction for " ++ (show t) ++ ":" ++ (show a)
  show (Ded (Unary d1) ctx t a) = "Unary deduction for " ++ (show t) ++ ":" ++ (show a)
  show (Ded (Binary d1 d2) ctx t a) = "Binary deduction for " ++ (show t) ++ ":" ++ (show a)

-- A simple deduction of the form {x:A} |-> x:A
var_deduction :: Char -> Type -> Deduction
var_deduction x t =
  let ctx = Ctx $ Map.singleton x t
  in Ded Axiom ctx (Var x) t