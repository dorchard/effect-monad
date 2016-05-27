{-# LANGUAGE RebindableSyntax, TypeOperators, DataKinds, KindSignatures, GADTs,
             TypeFamilies, UndecidableInstances #-}

import Prelude hiding (Monad(..))
import Control.Effect
import Control.Effect.CounterNat

import GHC.TypeLits

import Debug.Trace

{-

The 'Counter' graded monad is useful for counting computations
of a particular kind (e.g., counting number of calls to a websocket)
or estimating resource usage (e.g., a websocket call is more expensive
than a disk write).

By default, zero counts are tracked, e.g., -}

foo :: Counter 0 Int
foo = do x <- return 2
         y <- return 4
         return (x + y)

{- the 'one' function lifts a value to be counted once, e.g.  -}

-- foo2 :: Counter (S Z) Int
foo2 = do x <- tick 2
          y <- return 3
          return (x * y)

{- This can be used for other cool things, like proving that 'map' has
 linear complexity of 'map' at the type-level!

For this we need sized lists:
-}


data Vector (n :: Nat) a where
    Nil :: Vector 0 a
    Cons :: a -> Vector n a -> Vector (n + 1) a

type family (n :: Nat) :* (m :: Nat) :: Nat where
       0 :* m = 0
       n :* m = m + ((n - 1) :* m)

{- map' is then defined as follows -}

{- CAN'T TYPE CHECK, see Counter.hs

map' :: (a -> Counter t b) -> Vector n a -> Counter (n :* t) (Vector n b)
map' f Nil         = return Nil
map' f (Cons x xs) = do x' <- f x
                        xs' <- map' f xs
                        return (Cons x' xs')

-}
