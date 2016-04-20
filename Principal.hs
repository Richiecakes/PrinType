-------------------------------------------------------------
-- PRINCIPAL
-- Module implementing the principal type deduction algorithm
-- for finding most general types of lambda terms.
-- Type contexts.
-- Deductions.
-------------------------------------------------------------

module Principal(
  Deduction, Premise(..), principal_type_deduction, principal_type_deduction_log,
  d_premise, d_context, d_subject, d_type,
  Context, cmap, to_list,
  TypingFailure) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy
import Control.Monad.Error
import Control.Monad.Identity
import Data.List (intersperse)
import Data.Maybe (mapMaybe)

import Lambda
import Types
import Unify

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

-- PRINCIPAL TYPING --

data TypingFailure = Untypable String | Err String

instance Error TypingFailure

instance Show TypingFailure where
  show (Untypable s) = "Untypable: " ++ s
  show (Err s) = "Error: " ++ s 

data TypingState = TSt { freshVar :: Type
                       , currentStep :: Int
                       } deriving (Show)

next_fresh :: TypingState -> TypingState
next_fresh st =
  let (TVar x) = freshVar st
      step = currentStep st
  in TSt { freshVar = (TVar (x+1)), currentStep = step }

next_step :: TypingState -> TypingState
next_step st =
  let t = freshVar st
      step = (currentStep st) + 1
  in TSt { freshVar = t, currentStep = step }

type DeductionAlg a = StateT TypingState (ErrorT TypingFailure
                                         (WriterT [String] Identity)) a

runDeductionAlg :: DeductionAlg a -> (Either TypingFailure a, [String])
runDeductionAlg da = runIdentity (runWriterT (runErrorT (evalStateT da intialState)))
  where intialState = TSt { freshVar = (TVar 0), currentStep = 1 }

-- Get fresh type variable.
get_fresh_typevar :: DeductionAlg Type
get_fresh_typevar =
  do st <- get
     put $ next_fresh st
     return (freshVar st)

get_next_step :: DeductionAlg Int
get_next_step =
  do st <- get
     put $ next_step st
     return (currentStep st)

-- Perform principal type deduction on t.
principal_type_deduction :: Term -> Either TypingFailure Deduction
principal_type_deduction = fst . principal_type_deduction_log

principal_type_deduction_log :: Term -> (Either TypingFailure Deduction, [String])
principal_type_deduction_log t = runDeductionAlg (principal_type t)

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

principal_type_var :: Char -> DeductionAlg Deduction 
principal_type_var x =
  do t <- get_fresh_typevar
     return $ var_deduction x t

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

unify_deductions :: Substitution -> Type -> Deduction -> Deduction -> DeductionAlg Deduction
unify_deductions u a (Ded d1 ctx1 t1 a1) (Ded d2 ctx2 t2 a2) = 
  let ctx1' = cmap (applysub u) ctx1
      ctx2' = cmap (applysub u) ctx2
      ctx = context_union ctx1' ctx2'
      ded1' = applysub u (Ded d1 ctx1 t1 a1)
      ded2' = applysub u (Ded d2 ctx2 t2 a2)
      in return $ Ded (Binary ded1' ded2') ctx (app t1 t2) (applysub u a)

principal_type_failure :: Term -> Term -> DeductionAlg a
principal_type_failure t1 t2 =
  throwError $ Untypable $ "Cannot unify principal type deductions of " ++ (show t1) ++ " and " ++ (show t2) ++ " in their application " ++ (show $ app t1 t2)
