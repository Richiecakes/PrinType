{-|
Module      : Printype.Lambda.Combinators
Description : Commonly used combinators in the lambda calculus.
Copyright   : Richard Appleby 2016

Examples of both typbable and untypbable terms in the lambda calculus.
Includes SK combinators, boolean logic combinators and church numerals.
-}

module Printype.Lambda.Combinators(sCombinator, kCombinator, identity,
	numeral, true, false, andCombinator, orCombinator,
	twicetwice, twicetwicetwice,
	selfapply, abcd) where

import Printype.Lambda.Syntax
import Printype.Lambda.Parser

identity :: Term
identity = parseTerm "Lx.x"

sCombinator :: Term
sCombinator = parseTerm "Lxyz.xz(yz)"

kCombinator :: Term
kCombinator = parseTerm "Lxy.x"

true :: Term
true = kCombinator

false :: Term
false = parseTerm "Lxy.y"

andCombinator :: Term
andCombinator = lams "ab" $ appL [var 'a', var 'b', false]

orCombinator :: Term
orCombinator = lams "ab" $ appL [var 'a', true, var 'b']

abcd :: Term
abcd = parseTerm "abcd"

-- Given a positive integer n, returns the nth Church numeral.
numeral :: Int -> Term
numeral n
  | n < 0     = error "numeral must take positive integer"
  | otherwise = lams "fx" (nfoldapp n (var 'f') (var 'x'))

twice :: Term
twice = numeral 2

twicetwice :: Term
twicetwice = app twice twice

twicetwicetwice :: Term
twicetwicetwice = app twice twicetwice

selfapply :: Term
selfapply = parseTerm "Lx.xx"