# PrinType (WIP)

**Generates formal proofs for principal type deductions of terms in the simply typed lambda calculus.**

![deduction proof](https://raw.githubusercontent.com/Richiecakes/PrinType/master/image.png "Sample deduction proof")

> The study of the λ-calculus is of the set of terms and equations between the terms.

> ... a set of abstract types and a system for showing that terms
of the λ-calculus have a certain type. A term s of type A ⇒ B works like a function from
terms of type A to terms of type B, and we will only allow an application st if t is a term
of the correct “input” type A.

> ... an
algorithm which decides whether a term is typable, and finding a “most general” type if
it is. The most general types are called principal types and all the types of a particular
term will be instances of the principal type.

#### Building the source using cabal
  * Clone the repo and cd into it
  * Build into a sandbox
   
    ```bash
    $ cabal sandbox init                   # Initialise the sandbox
    $ cabal install --only-dependencies    # Install dependencies into the sandbox
    $ cabal build  
    ```
    
#### Building the docs using cabal and haddock
  * Clone the repo and cd into it
  * Install the dependencies into a sandbox with documentation and run haddock
   
    ```bash
    $ cabal sandbox init                   # Initialise the sandbox
    $ cabal install -j --only-dep --enable-documentation
    $ cabal haddock  
    ```
