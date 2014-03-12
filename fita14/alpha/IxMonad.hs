{-# LANGUAGE KindSignatures, TypeFamilies #-}

module IxMonad where 

import Prelude hiding (Monad(..),(>>))

class IxMonad (m :: * -> * -> *) where

   type Unit m 
   type Plus m s t

   return :: a -> m (Unit m) a
   (>>=) :: m s a -> (a -> m t b) -> m (Plus m s t) b

x >> y = x >>= (\_ -> y)
fail = undefined


