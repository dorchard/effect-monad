{-# LANGUAGE ConstraintKinds, TypeFamilies, KindSignatures #-}

module IxMonad where 

import Prelude hiding ((>>=), return)
import GHC.Prim

class IxMonad (m :: * -> * -> *) where

   type Inv m s t :: Constraint
   type Inv m s t = ()

   type Unit m 
   type Plus m s t

   return :: a -> m (Unit m) a
   (>>=) :: Inv m s t => m s a -> (a -> m t b) -> m (Plus m s t) b

fail = undefined


