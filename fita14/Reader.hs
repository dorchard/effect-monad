{-# LANGUAGE TypeFamilies, RebindableSyntax, FlexibleInstances #-}

import Prelude hiding (Monad(..))
import IxMonad

import Data.Char

instance IxMonad (->) where
    type Unit (->) = ()
    type Plus (->) s t = (s, t)

    return x  = \() -> x
    f >>= k   = \(s, t) -> (k (f s)) t

ask :: p -> p
ask = id

foo = do x <- ask 
         xs <- ask
         return (x : xs)

foo_eval = foo ('a', ("bc", ()))

