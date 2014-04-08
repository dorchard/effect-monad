{-# LANGUAGE TypeFamilies, EmptyDataDecls, TypeOperators, DataKinds #-}

module Control.IxMonad.Counter where

import Control.IxMonad
import Prelude hiding (Monad(..))
import GHC.TypeLits

data Counter (n :: Nat) a = Counter { forget :: a }
instance IxMonad Counter where
    type Unit Counter = 0
    type Plus Counter n m = n + m
    type Inv  Counter n m = ()

    return a = Counter a
    (Counter a) >>= k = Counter . forget $ k a

one :: a -> Counter 1 a
one x = Counter x
