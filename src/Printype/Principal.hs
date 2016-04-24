{-|
Module      : Printype.Principal
Description : Principal type deduction algorithm.
Copyright   : Richard Appleby 2016

Module implementing the principal type deduction algorithm for finding most general types of lambda terms.
-}

module Printype.Principal(
  -- * Typing functions
  principal_type_deduction,
  principal_type_deduction_log,
  is_typbable,
  -- * Types
  Deduction(..),
  Premise(..),
  TypingFailure(..),
  Context,
  -- * Helper functions
  to_list,
  -- * Re-exports
  module Printype.Lambda,
  module Printype.Types) where

import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy
import Control.Monad.Error
import Control.Monad.Identity
import Data.Maybe (mapMaybe)
import Data.Either (isRight)

import Printype.Lambda
import Printype.Types
import Printype.Principal.Contexts
import Printype.Principal.Deductions


-- |Performs principal type deduction on a term.
principal_type_deduction :: Term                             -- ^ The term to type.
                         -> Either TypingFailure Deduction   -- ^ Either a proof demonstrating the terms principal type or an explanation as to why the term is not typbable.
principal_type_deduction = fst . principal_type_deduction_log



-- |As above, but also returns a textual log of the algorithms operation.
principal_type_deduction_log :: Term -> (Either TypingFailure Deduction, [String])
principal_type_deduction_log t = runDeductionAlg (principal_type t)

is_typbable :: Term -> Bool
is_typbable = isRight .principal_type_deduction

-- |Output of typing algorithm when it fails
data TypingFailure 
  = Untypable String -- ^ The term was untypable for the given reason.
  | Err String       -- ^ The typing algorithm encountered the given runtime error.

instance Error TypingFailure

instance Show TypingFailure where
  show (Untypable s) = "Untypable: " ++ s
  show (Err s) = "Error: " ++ s 

-- IMPLEMENTATION --

-- |State of typing algorithm
data TypingState = TSt { freshVar :: Type    -- ^ Carry around a fresh type variable to create new types.
                       , currentStep :: Int  -- ^ Count the number of deduction steps.
                       } deriving (Show)

-- |Update the state with a fresh type variable.
next_fresh :: TypingState -> TypingState
next_fresh st =
  let (TVar x) = freshVar st
      step = currentStep st
  in TSt { freshVar = (TVar (x+1)), currentStep = step }

-- |Increment the step counter.
next_step :: TypingState -> TypingState
next_step st =
  let t = freshVar st
      step = (currentStep st) + 1
  in TSt { freshVar = t, currentStep = step }


{-|
Monad transformer stack for typing algorithm. We will use
  * StateT, for keeping track of a fresh type variable and step counter.
    Use State as the bottom monad as we have no need to carry state once typing has failed.
  * ErrorT, for propagating typing failues.
  * WriterT for logging operation of algorithm. Sits at top of stack as we always want the log returned.
-}
type DeductionAlg a = StateT TypingState (ErrorT TypingFailure
                                         (WriterT [String] Identity)) a


-- |Runs a DeductionAlg monad and returns the result and the log.
runDeductionAlg :: DeductionAlg a -> (Either TypingFailure a, [String])
runDeductionAlg da = runIdentity (runWriterT (runErrorT (evalStateT da intialState)))
  where intialState = TSt { freshVar = (TVar 0), currentStep = 1 }

-- |Get fresh type variable from state.
get_fresh_typevar :: DeductionAlg Type
get_fresh_typevar =
  do st <- get
     put $ next_fresh st
     return (freshVar st)

-- |Get and increment the step counter in the state.
get_next_step :: DeductionAlg Int
get_next_step =
  do st <- get
     put $ next_step st
     return (currentStep st)

-- |Entry point to the typing algorithm implementation.
principal_type :: Term -> DeductionAlg Deduction
principal_type t =
  let dm = case t of (Var x) -> principal_type_var x
                     (Lam x t) -> principal_type_abs x t
                     (App t1 t2) -> principal_type_app t1 t2
  in do
       x <- dm
       st <- get_next_step
       tell $ ["Step " ++ show st ++ ": " ++ (show t) ++ " " ++ (show x)]
       return x

-- |Types a term of the form x
principal_type_var :: Char -> DeductionAlg Deduction 
principal_type_var x =
  do t <- get_fresh_typevar
     return $ var_deduction x t

-- |Types a term of the form Lx.t
principal_type_abs :: Char -> Term -> DeductionAlg Deduction 
principal_type_abs x t =
	do
    (Ded d ctx _ a) <- principal_type t
    if x `free_in` t then
      case type_in_context ctx x of
        Just a1 -> return $ Ded (Unary (Ded d ctx t a)) (ctx \\\ x) (lam x t) (a1 :=> a)
        Nothing -> throwError $ Err "x free in t but does not appear in type context"
		else 
      do
        a1 <- get_fresh_typevar
        return $ Ded (Unary (Ded d ctx t a)) ctx (lam x t) (a1 :=> a)

-- |Types a term of the form (uv)
principal_type_app :: Term -> Term -> DeductionAlg Deduction 
principal_type_app t1 t2 = 
  do 
    (Ded d1 ctx1 _ a1) <- principal_type t1
    (Ded d2 ctx2 _ a2) <- principal_type t2
    let cvars = common_vars ctx1 ctx2
        cs = mapMaybe (type_in_context ctx1) cvars
        ds = mapMaybe (type_in_context ctx2) cvars
        in do 
             a3 <- if (is_arrow_type a1) then return (restype a1)
                   else get_fresh_typevar
             let cs' = a1:cs
                 ds' = (a2 :=> a3):ds
                 mgu = mgu_sequence cs' ds'
                 in case mgu of
                      Nothing -> principal_type_failure t1 t2
                      Just u -> unify_deductions u a3 (Ded d1 ctx1 t1 a1) (Ded d2 ctx2 t2 a2)

-- |Helper function to apply a unifier to a binary deduction step.
unify_deductions :: Substitution -> Type -> Deduction -> Deduction -> DeductionAlg Deduction
unify_deductions u a (Ded d1 ctx1 t1 a1) (Ded d2 ctx2 t2 a2) = 
  let ctx1' = cmap (applysub u) ctx1
      ctx2' = cmap (applysub u) ctx2
      ctx = context_union ctx1' ctx2'
      ded1' = applysub u (Ded d1 ctx1 t1 a1)
      ded2' = applysub u (Ded d2 ctx2 t2 a2)
      in return $ Ded (Binary ded1' ded2') ctx (app t1 t2) (applysub u a)

-- |Helper function for throwing Untypbable errors.
principal_type_failure :: Term -> Term -> DeductionAlg a
principal_type_failure t1 t2 =
  throwError $ Untypable $ "Cannot unify principal type deductions of " ++ (show t1) ++ " and " ++ (show t2) ++ " in their application " ++ (show $ app t1 t2)
