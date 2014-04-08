{-# LANGUAGE KindSignatures, TypeFamilies, ConstraintKinds, PolyKinds #-}

module Control.IxMonad where 

import Prelude hiding (Monad(..))
import GHC.Prim

class IxMonad (m :: k -> * -> *) where

   type Unit m :: k
   type Plus m (s :: k) (t :: k) :: k

   type Inv m (s :: k) (t :: k) :: Constraint
   type Inv m s t = ()

   return :: a -> m (Unit m) a
   (>>=) :: Inv m s t => m s a -> (a -> m t b) -> m (Plus m s t) b

   (>>) :: Inv m s t => m s a -> m t b -> m (Plus m s t) b
   x >> y = x >>= (\_ -> y)
  
fail = undefined


