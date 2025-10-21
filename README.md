Graded monads are a generalisation of monads where, from a functional
programming perspective, a monadic type constructor is annotated
(indexed) by another type representing effect information that
explains the effects of a computation, i.e., values of type `M r a`
are effectul computations of values of type `a` with effects `r`. They
provide a way to give finer-grained information about impurity and
capture a greater class of effectul computations that normal monads.

This library is based on the paper "Embedding Effect Systems in
Haskell" (Orchard, Petricek, Haskell 2014). For more information, see
talk slides and papers here:

       http://www.cs.kent.ac.uk/~dao7/publ/haskell14-effects.pdf
       http://www.cs.kent.ac.uk/~dao7/talks/haskell14-orchard-petricek-effects.pdf

This is also available on hackage (http://hackage.haskell.org/package/effect-monad)
so you can get this by doing:

       cabal install effect-monad
