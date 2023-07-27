{-# LANGUAGE TypeFamilies, EmptyDataDecls, TypeOperators, DataKinds #-}

module Control.Effect.Counter(N, Counter, tick, (:+)) where

import Control.Effect
import Prelude hiding (Monad(..))

{-| Provides a way to 'count' in the type-level with a monadic interface
    to sum up the individual counts of subcomputations -}

{-| Define type constructors for natural numbers -}
data N = Z | S N

{-| The counter has no semantic meaning -}
data Counter (n :: N) a = Counter { forget :: a }

{-| Type-level addition -}
type family (n :: N) :+ (m :: N) where
            n :+ Z     = n
            n :+ (S m) = S (n :+ m)

instance Effect Counter where
    type Inv Counter n m = ()

    {-| Trivial effect annotation is 0 -}
    type Unit Counter = Z
    {-| Compose effects by addition -}
    type Plus Counter n m = n :+ m

    return a = Counter a
    (Counter a) >>= k = Counter . forget $ k a

{-| A 'tick' provides a way to increment the counter -}
tick :: a -> Counter (S Z) a
tick x = Counter x

    