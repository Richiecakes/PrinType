{-|
Module      : Printype.Types
Description : Simple types for the lambda calculus.
Copyright   : Richard Appleby 2016

A single import location for everything relating to simple types.
-}

module Printype.Types(
  module Printype.Types.Syntax,
  module Printype.Types.Unify,
  Substitution,
  Substitutable,
  apply,
  subjects
  ) where


import Printype.Types.Syntax
import Printype.Types.Unify
import Printype.Types.Substitutions (Substitution, Substitutable, apply, subjects)