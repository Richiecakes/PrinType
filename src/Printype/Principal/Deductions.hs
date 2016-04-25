{-|
Module      : Printype.Principal.Deductions
Description : Implements a syntax for type deduction trees.
Copyright   : Richard Appleby 2016

A deduction tree is a formal proof tree describing how a particular type assignment is derived.
-}

module Printype.Principal.Deductions(
  Deduction,
  premise,
  context,
  subject,
  ptype,
  Premise(..),
  axiom,
  abstract,
  unify) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Printype.Lambda
import Printype.Types
import Printype.Principal.Contexts as Ctx
import Printype.Types.Substitutions as Sub

-- DEDUCTIONS --

data Deduction = Ded { d_premise :: Premise
                     , d_context :: Context
                     , d_subject :: Term
                     , d_type    :: Type
                     }

premise :: Deduction -> Premise
premise (Ded d _ _ _) = d

context :: Deduction -> Context
context (Ded _ ctx _ _) = ctx

subject :: Deduction -> Term
subject (Ded _ _ t _) = t

ptype :: Deduction -> Type
ptype (Ded _ _ _ a) = a

data Premise = Axiom | Unary Deduction | Binary Deduction Deduction

instance Substitutable Deduction where
  apply c (Ded Axiom ctx t a) = Ded Axiom (apply c ctx) t (apply c a)
  apply c (Ded (Unary d1) ctx t a) = Ded (Unary (apply c d1)) (apply c ctx) t (apply c a)
  apply c (Ded (Binary d1 d2) ctx t a) = Ded (Binary (apply c d1) (apply c d2)) (apply c ctx) t (apply c a)

  subjects (Ded Axiom ctx t a) = Set.union (subjects ctx) (subjects a)
  subjects (Ded (Unary d1) ctx t a) = Set.unions [subjects d1, subjects ctx, subjects a]
  subjects (Ded (Binary d1 d2) ctx t a) = Set.unions [subjects d1, subjects d2, subjects ctx, subjects a]

instance Show Deduction where
  show (Ded Axiom ctx t a) = "Axiomatic deduction for " ++ (show t) ++ ":" ++ (show a)
  show (Ded (Unary d1) ctx t a) = "Unary deduction for " ++ (show t) ++ ":" ++ (show a)
  show (Ded (Binary d1 d2) ctx t a) = "Binary deduction for " ++ (show t) ++ ":" ++ (show a)

-- A simple deduction of the form {x:A} |-> x:A
axiom :: Char -> Type -> Deduction
axiom x t =
  let ctx = Ctx.singleton x t
  in Ded Axiom ctx (Var x) t

abstract :: Deduction -> Char -> Type -> Deduction
abstract (Ded d ctx t a) x a1 = 
  Ded (Unary (Ded d ctx t a)) (Ctx.remove ctx x) (lam x t) (a1 :=> a)

unify :: Deduction -> Deduction -> Sub.Substitution -> Deduction
unify (Ded d1 ctx1 t1 a1) (Ded d2 ctx2 t2 a2) u = 
  let ctx1' = Sub.apply u ctx1
      ctx2' = Sub.apply u ctx2
      ctx = Ctx.union ctx1' ctx2'
      ded1' = Sub.apply u (Ded d1 ctx1 t1 a1)
      ded2' = Sub.apply u (Ded d2 ctx2 t2 a2)
      a1' = Sub.apply u a1
  in Ded (Binary ded1' ded2') ctx (app t1 t2) (Sub.apply u (restype a1'))