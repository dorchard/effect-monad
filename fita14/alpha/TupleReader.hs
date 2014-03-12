{-# LANGUAGE TypeFamilies, RebindableSyntax, FlexibleInstances #-}

import Prelude hiding (Monad(..))
import IxMonad

import Data.Char

instance IxMonad (->) where  -- M r a = r -> a
    -- return :: a -> m (Unit m) a
    -- return :: a -> (() -> a)
    return x  = \() -> x

    type Unit (->) = ()

    type Plus (->) s t = (s, t)
    -- (>>=) :: m s a -> (a -> m t b) -> m (Plus m s t) b
    -- (>>=) :: (s -> a) -> (a -> (t -> b)) -> ((s, t) -> b)
    e >>= k   = \(s, t) -> (k (e s)) t

ask :: p -> p
ask = id

foo = do x <- ask 
         xs <- ask
         return (x : xs)

foo_eval = foo ('a', ("bc", ()))
