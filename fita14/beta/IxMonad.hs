{-# LANGUAGE KindSignatures, TypeFamilies, ConstraintKinds #-}

module IxMonad where 

import Prelude hiding ((>>=), return)
import GHC.Prim

class IxMonad (m :: * -> * -> *) where

   type Unit m 
   type Plus m s t

   type Inv m s t :: Constraint

   return :: a -> m (Unit m) a
   (>>=) :: Inv m s t => m s a -> (a -> m t b) -> m (Plus m s t) b

fail = undefined


