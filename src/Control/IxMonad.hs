{-# LANGUAGE KindSignatures, TypeFamilies, ConstraintKinds, PolyKinds, MultiParamTypeClasses #-}

module Control.IxMonad where 

import Control.IxMonad.Helpers.Set
import Prelude hiding (Monad(..))
import Data.Proxy
import GHC.Prim

{-
type InvMain m f g = (Plus m (Unit m) f ~ f,
                      Plus m f (Unit m) ~ f) 
                      Plus m f (Plus m g h) ~ Plus m (Plus m f g) h)
-}

class IxMonad (m :: k -> * -> *) where

   type Unit m :: k
   type Plus m (f :: k) (g :: k) :: k

   type Inv m (f :: k) (g :: k) :: Constraint
   type Inv m f g = ()

   return :: a -> m (Unit m) a
   (>>=) :: (Inv m f g) => m f a -> (a -> m g b) -> m (Plus m f g) b

   (>>) :: (Inv m f g) => m f a -> m g b -> m (Plus m f g) b
   x >> y = x >>= (\_ -> y)
  
fail = undefined

class Subeffect (m :: k -> * -> *) f g where
    sub :: m f a -> m g a