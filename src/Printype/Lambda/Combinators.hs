{-|
Module      : Printype.Lambda.Combinators
Description : Commonly used combinators in the lambda calculus.
Copyright   : Richard Appleby 2016

Examples of both typbable and untypbable terms in the lambda calculus.
Includes SK combinators, boolean logic combinators and church numerals.
-}

module Printype.Lambda.Combinators(
	-- $comb

	-- * SKI Calculus
	s,
	k,
	i,
	-- * Boolean Combinators
	true,
	false,
	and,
	or,
	-- * Church numerals
	numeral,
	-- * Others
	selfapply) where

import Printype.Lambda.Syntax
import Printype.Lambda.Parser

import Prelude hiding (and, or)

-- $comb
-- Standard combinators used in lambda calculus.
-- This module is intended to be imported qualified, e.g.
--
-- > import qualified Printype.Lambda.Combinators as C

-- |&#955;x.x
i :: Term
i = parseTerm "Lx.x"


-- |&#955;xyz.xz(yz)
s :: Term
s = parseTerm "Lxyz.xz(yz)"

-- |&#955;xy.x
k :: Term
k = parseTerm "Lxy.x"

{-|
Sometimes referred to as k, fst or const.

&#955;xy.x
-}
true :: Term
true = k

{-|
Sometimes referred to as snd or seq.

&#955;xy.y
-}
false :: Term
false = parseTerm "Lxy.y"

{-|
Boolean AND function defined for boolean terms above.

&#955;ab.ab&#955;xy.y
-}
and :: Term
and = lams "ab" $ appL [var 'a', var 'b', false]

{-|
Boolean OR function defined for boolean terms above.

&#955;ab.a(&#955;xy.x)b
-}
or :: Term
or = lams "ab" $ appL [var 'a', true, var 'b']

{-|
Given a positive integer n, returns the nth Church numeral.

&#955;fx.f(f(...f(x)...))
-}
numeral :: Int -> Term
numeral n
  | n < 0     = error "numeral must take positive integer"
  | otherwise = lams "fx" (nfoldapp n (var 'f') (var 'x'))


{-|
An example of a self application which is not typbable.

&#955;x.xx
-}
selfapply :: Term
selfapply = parseTerm "Lx.xx"