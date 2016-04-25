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
import qualified Printype.Principal.Contexts as C
import qualified Printype.Types.Substitutions as Sub

-- DEDUCTIONS --

data Deduction = Ded { d_premise :: Premise
                     , d_context :: C.Context
                     , d_subject :: Term
                     , d_type    :: Type
                     }

data Premise = Axiom | Unary Deduction | Binary Deduction Deduction

instance Sub.Substitutable Deduction where
  apply c (Ded Axiom ctx t a) = Ded Axiom (Sub.apply c ctx) t (Sub.apply c a)
  apply c (Ded (Unary d1) ctx t a) = Ded (Unary (Sub.apply c d1)) (Sub.apply c ctx) t (Sub.apply c a)
  apply c (Ded (Binary d1 d2) ctx t a) = Ded (Binary (Sub.apply c d1) (Sub.apply c d2)) (Sub.apply c ctx) t (Sub.apply c a)

  subjects (Ded Axiom ctx t a) = Set.union (Sub.subjects ctx) (Sub.subjects a)
  subjects (Ded (Unary d1) ctx t a) = Set.unions [Sub.subjects d1, Sub.subjects ctx, Sub.subjects a]
  subjects (Ded (Binary d1 d2) ctx t a) = Set.unions [Sub.subjects d1, Sub.subjects d2, Sub.subjects ctx, Sub.subjects a]

instance Show Deduction where
  show (Ded Axiom ctx t a) = "Axiomatic deduction for " ++ (show t) ++ ":" ++ (show a)
  show (Ded (Unary d1) ctx t a) = "Unary deduction for " ++ (show t) ++ ":" ++ (show a)
  show (Ded (Binary d1 d2) ctx t a) = "Binary deduction for " ++ (show t) ++ ":" ++ (show a)

-- A simple deduction of the form {x:A} |-> x:A
var_deduction :: Char -> Type -> Deduction
var_deduction x t =
  let ctx = C.singleton x t
  in Ded Axiom ctx (Var x) t