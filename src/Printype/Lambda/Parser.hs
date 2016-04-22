{-|
Module      : Printype.Lambda.Parser
Description : Simple parser for lambda calculus terms.

A simple parsing library and parser for fully bracketed lambda calculus terms
-}

module Printype.Lambda.Parser(
  parseTerm) where

import Data.Char (isLower)

import Printype.Lambda.Syntax

type Parse a = String -> Maybe (a, String)

token :: Char -> Parse Char
token c []     = Nothing
token c (x:xs) = if x==c then Just (c,xs) else Nothing

spot :: (Char -> Bool) -> Parse Char
spot p []     = Nothing
spot p (x:xs) = if p x then Just (x,xs) else Nothing

infixr 5 ^^^
(^^^) :: Parse a -> Parse b -> Parse (a,b)
(p1 ^^^ p2) inp =
  case p1 inp of
    Nothing -> Nothing
    Just (v,inp') -> case p2 inp' of
        Nothing -> Nothing
        Just (u,ys) -> Just ((v,u),ys)

infixl 4 >>>
(>>>) :: Parse a -> (a -> b) -> Parse b
(p >>> f) inp = case p inp of
                  Nothing -> Nothing
                  Just (v,xs) -> Just (f v, xs)

infixr 3 |||
(|||) :: Parse a -> Parse a -> Parse a
(p1 ||| p2) inp = case p1 inp of
                    Nothing -> p2 inp
                    Just (v,xs) -> Just (v,xs)

many :: Parse a -> Parse [a]
many p =
  p ^^^ many p >>> cons
  |||
  \inp -> Just ([], inp)

cons (x, xs) = x:xs

pTerm :: Parse Term
pTerm =
  token 'L' ^^^ pVars ^^^ token '.' ^^^ pTerm >>> mkLam
  |||
  many pTerm' >>> appL
    where mkLam (_, (xs, (_, t))) = lams xs t

pTerm' :: Parse Term
pTerm' =
  pVar >>> var
  |||
  pParens

pVar :: Parse Char
pVar =
  spot isLower

pVars :: Parse [Char]
pVars =
  many pVar

pParens :: Parse Term
pParens =
  token '(' ^^^ pTerm ^^^ token ')' >>> remPars
    where remPars (_, (t, _)) = t

topLevel :: Parse a -> String -> a
topLevel p inp =
  case p inp of
    Just (result,"") -> result
    Just (result,rest) ->
      error ("parse failed; input unconsumed: " ++
             rest)
    Nothing -> error "parse unsuccessful"

parseTerm :: String -> Term
parseTerm = topLevel pTerm