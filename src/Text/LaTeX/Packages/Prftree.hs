{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Text.LaTeX.Packages.Prftree
Description : A helper library for using the 'prftree' package with HaTeX.
Copyright   : Richard Appleby 2016

Provides useful command bindings and package name for easily constructing prftree
proofs with HaTeX.
-}

module Text.LaTeX.Packages.Prftree(rightarrow, prftree,
  axiom, unary, binary,
  namedAxiom, namedUnary, namedBinary) where

import Text.LaTeX
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Class

prftree :: PackageName
prftree = "prftree"

axiom :: LaTeXC l => l -> l
axiom = comm1 "prfaxiom"

namedAxiom :: LaTeXC l => l -> l -> l
namedAxiom = comm2 "prfbyaxiom"

unary :: LaTeXC l => l -> l -> l
unary = comm2 "prftree"

namedUnary :: LaTeXC l => l -> l -> l -> l
namedUnary = liftL3 $ \name ax l -> TeXComm "prftree" [OptArg "r", FixArg name, FixArg ax, FixArg l]

binary :: LaTeXC l => l -> l -> l -> l
binary = comm3 "prftree"

namedBinary :: LaTeXC l => l -> l -> l -> l -> l
namedBinary = liftL4 $ \name ax1 ax2 l -> TeXComm "prftree" [OptArg "r", FixArg name, FixArg ax1, FixArg ax2, FixArg l]

rightarrow :: LaTeXC l => l
rightarrow = comm0 "Rightarrow"

comm2 :: LaTeXC l => String -> l -> l -> l
comm2 str = liftL2 $ \l1 l2 -> TeXComm str [FixArg l1, FixArg l2]

comm3 :: LaTeXC l => String -> l -> l -> l -> l
comm3 str = liftL3 $ \l1 l2 l3 -> TeXComm str [FixArg l1, FixArg l2, FixArg l3]

liftL4 :: LaTeXC l => (LaTeX -> LaTeX -> LaTeX -> LaTeX -> LaTeX) -> l -> l -> l -> l -> l
liftL4 f u x y z = liftListL (\[u,x,y,z] -> f u x y z) [u,x,y,z]