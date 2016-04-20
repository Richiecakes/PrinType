# PrinType (WIP)
Generates formal proofs for principal type deductions of terms in the simply typed lambda calculus.

> The study of the λ-calculus is of the set of terms and equations between the terms.

> ... a set of abstract types and a system for showing that terms
of the λ-calculus have a certain type. A term s of type A ⇒ B works like a function from
terms of type A to terms of type B, and we will only allow an application st if t is a term
of the correct “input” type A.

> ... an
algorithm which decides whether a term is typable, and finding a “most general” type if
it is. The most general types are called principal types and all the types of a particular
term will be instances of the principal type.

#### Dependencies
Uses the HaTeX library. Produced LaTeX uses the prftree package.

#### Module Summary
##### Lambda
Datatypes and helper functions for constructing and analysing terms of the lambda calculus.
#####  Combinators
Provides some commonly used terms.
#####  Types
Datatypes and helper functions for types and type substitutions.
##### Unify
Implements Robinson's type unification algorithm.
#####  Principal
Implements the simple principal typing algorithm
#####  Latex
Provides a HaTeX Powered LaTeX pretty printer for formal proofs of deductions, using the "prftree" package.
#####  Prftree
A HaTeX helper file for using the "prftree" package
