{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Printype.Latex
Description : A LaTeX based pretty printer for type deductions.
Copyright   : Richard Appleby 2016

A LaTeX based pretty printer for type deductions using the HaTeX library and the prftree LaTeX package.
-}

module Printype.Latex(render_term_deduction, render_term_deductions) where

import Printype.Principal
import Printype.Principal.Contexts (toList)

import Text.LaTeX.Packages.Prftree

import Data.List (intersperse)
import Data.Version (showVersion)

import qualified System.Info as Sys

import Text.LaTeX
import Text.LaTeX.Packages.AMSMath

instance Texy Deduction where
  texy d =
    let l = texy (context d)
         <> mapsto
         <> texy (subject d)
         <> ":"
         <> texy (ptype d)
    in case (premise d) of
         Axiom -> namedAxiom "(var)" l
         Unary d1 -> namedUnary "(abs)" (texy d1) l
         Binary d1 d2 -> namedBinary "(app)" (texy d1) (texy d2) l

instance Texy Term where
  texy (Var x) = fromString [x]
  texy (Lam x (Lam y t)) = lambda <> (fromString [x, y]) <> "." <> (texy t)
  texy (Lam x t) = lambda <> (fromString [x]) <> "." <> (texy t)
  texy (App t1 (App t2 t3)) = (texy t1) <> "(" <> (texy t2) <> (texy t3) <> ")"
  texy (App (App t1 t2) t3) = (texy t1) <> (texy t2) <> (texy t3)
  texy (App t1 t2) = (texy t1) <> (texy t2)

instance Texy Context where
  texy c = 
    let pairs = toList c
        pairsl = map (\ (c, t) -> fromString [c] <> ":" <> texy t) pairs
        pairslc = intersperse (fromString ",") pairsl
    in "{" <> (mconcat pairslc) <> "}"

instance Texy Type where
  texy (TVar a) = texy a
  texy ((t1 :=> t2) :=> t3) = "(" <> (texy (t1 :=> t2)) <> ")" <> rightarrow <> (texy t3)
  texy (t1 :=> t2) = (texy t1) <> rightarrow <> (texy t2)

render_term_deduction :: Term -> IO ()
render_term_deduction t = render_term_deductions [t]

render_term_deductions :: [Term] -> IO ()
render_term_deductions ts = execLaTeXT (latex_term_deductions ts) >>= renderFile "simple.tex"

latex_term_deductions :: Monad m => [Term] -> LaTeXT_ m
latex_term_deductions ts = 
  let tsrs = zip ts $ map principal_type_deduction ts
  in do thePreamble
        document $ theBody tsrs

-- Preamble with some basic info.
thePreamble :: Monad m => LaTeXT_ m
thePreamble = do
 documentclass [] article
 usepackage [] amsmath
 usepackage ["landscape"] "geometry"
 usepackage [] prftree

-- Body with a section.
theBody :: Monad m => [(Term, Either TypingFailure Deduction)] -> LaTeXT_ m
theBody tsrs = do
  theAbstract
  tableofcontents
  (mconcat . map theSection) tsrs

theAbstract :: Monad m => LaTeXT_ m
theAbstract = do "Principal typing derivation(s) generated by "
                 textbf "PrinType"
                 " on "
                 today
                 " using "
                 hatex_version
                 par
                 "Host info: "
                 fromString Sys.os
                 "---"
                 fromString Sys.arch
                 "---"
                 fromString Sys.compilerName
                 "---"
                 (fromString . showVersion) Sys.compilerVersion


theSection :: Monad m => (Term, Either TypingFailure Deduction) -> LaTeXT_ m
theSection (t, r) = 
  case r of
    Right d  -> do section $ "Deduction for " <> (math . texy) t
                   mathDisplay $ texy d
    Left err -> do section $ "Deduction for " <> (math . texy) t
                   (fromString . show) err