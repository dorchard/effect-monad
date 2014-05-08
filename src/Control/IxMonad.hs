{-# LANGUAGE KindSignatures, TypeFamilies, ConstraintKinds, PolyKinds #-}

module Control.IxMonad where 

import Control.IxMonad.Helpers.Set
import Prelude hiding (Monad(..))
import GHC.Prim

class IxMonad (m :: k -> * -> *) where

   type Unit m :: k
   type Plus m (f :: k) (g :: k) :: k

   type Inv m (f :: k) (g :: k) :: Constraint
   type Inv m f g = ()

   return :: a -> m (Unit m) a
   (>>=) :: Inv m f g => m f a -> (a -> m g b) -> m (Plus m f g) b

   (>>) :: Inv m f g => m f a -> m g b -> m (Plus m f g) b
   x >> y = x >>= (\_ -> y)
  
fail = undefined


