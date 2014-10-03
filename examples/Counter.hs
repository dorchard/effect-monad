{-# LANGUAGE RebindableSyntax, EmptyDataDecls, GADTs, TypeFamilies, UndecidableInstances, MultiParamTypeClasses, TypeOperators #-}

import Prelude hiding (Monad(..))
import Control.Effect
import Control.Effect.Counter

import Debug.Trace

{- 

The 'Counter' indexed monad is useful for counting computations
of a particular kind (e.g., counting number of calls to a websocket)
or estimating resource usage (e.g., a websocket call is more expensive
than a disk write). 

By default, zero counts are tracked, e.g., 

-}

foo :: Counter Z Int
foo = do x <- return 2
         y <- return 4
         return (x + y)

{- the 'tick' function lifts a value to be counted once, e.g. -}

-- foo2 :: Counter (S Z) Int
foo2 = do x <- tick 2
          y <- return 3
          return (x * y)

{- This can be used for other cool things, like proving that 'map' has 
 linear complexity of 'map' at the type-level! 

For this we need sized lists:
-}

data Vector n a where
    Nil :: Vector Z a
    Cons :: a -> Vector n a -> Vector (S n) a

type family n :* m where
            Z     :* m = Z
            (S n) :* m = m :+ (n :* m)

{- map' is then defined as follows -}

map' :: (a -> Counter t b) -> Vector n a -> Counter (n :* t) (Vector n b)
map' f Nil         = return Nil
map' f (Cons x xs) = do x' <- f x
                        xs' <- map' f xs
                        return (Cons x' xs')

{- The types show us that if the function counts 't' things, then applying 'map'
to an n-vector counts 'tn' things -}

{- Example: web socket calls- how many do we do per instances #-}

call :: Int -> Counter (S Z) ()
call = undefined

singleCall = map' call (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil))))

doubleCall x = map' (\n -> do {a <- call n; b <- call n; return ()}) x

doubleCallExample = doubleCall (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil))))




{- are we definitely linear in the number of elements, even if we have closed over the vector? -}

class LT n m
instance LT Z (S n)
instance LT n m => LT (S n) (S m)

lineraMap :: LT t n => (a -> Counter t b) -> Vector n a -> Counter (n :* t) (Vector n b)
lineraMap = map'