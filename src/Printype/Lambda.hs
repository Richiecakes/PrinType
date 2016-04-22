{-|
Module      : Printype.Lambda
Description : Syntax and parser for lambda calculus terms.
Copyright   : Richard Appleby 2016

A single import location for everything relating to terms of the untyped lambda calculus.
-}

module Printype.Lambda(
  module Printype.Lambda.Syntax,
  module Printype.Lambda.Parser
  ) where

import Printype.Lambda.Syntax
import Printype.Lambda.Parser