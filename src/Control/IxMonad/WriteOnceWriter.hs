{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, 
             TypeOperators, DataKinds, KindSignatures #-}

module Control.IxMonad.WriteOnceWriter (put, WriteOnce(..)) where

import Control.IxMonad
import Control.IxMonad.Helpers.List
import Prelude hiding (Monad(..))

data WriteOnce (w :: [*]) a = W { runWriteOnce :: (a, List w) }

instance IxMonad WriteOnce where
    type Unit WriteOnce = '[]
    type Plus WriteOnce s t = s :++ t

    return x = W (x, Nil)
    (W (a, r)) >>= k = let (W (b, s)) = k a in W (b, r `append` s)
 
put :: a -> WriteOnce (a ': '[]) ()
put x = W ((), Cons x Nil)



