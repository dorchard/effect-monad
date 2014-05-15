{-# LANGUAGE TypeFamilies, TypeOperators, DataKinds #-}

module Control.Effect.CounterNat where

import Control.Effect
import GHC.TypeLits
import Prelude hiding (Monad(..))

{-| Provides a way to 'count' in the type-level with a monadic interface
    to sum up the individual counts of subcomputations. Instead 
    of using our own inductive natural number typ, this uses the 'Nat' kind from 'GHC.TypeLits' -}

{-| The counter has no semantic meaning -}
data Counter (n :: Nat) a = Counter { forget :: a }

instance Effect Counter where
    type Inv Counter n m = ()
    {-| Trivial effect annotation is 0 -}
    type Unit Counter = 0
    {-| Compose effects by addition -}
    type Plus Counter n m = n + m

    return a = Counter a   
    (Counter a) >>= k = Counter . forget $ k a

{-| A 'tick' provides a way to increment the counter -}
tick :: a -> Counter 1 a
tick x = Counter x
