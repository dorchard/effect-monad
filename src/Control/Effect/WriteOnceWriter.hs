{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, 
             TypeOperators, DataKinds, KindSignatures #-}

module Control.Effect.WriteOnceWriter (put, WriteOnce(..)) where

import Control.Effect
import Control.Effect.Helpers.List
import Prelude hiding (Monad(..))

data WriteOnce (w :: [*]) a = W { runWriteOnce :: (a, List w) }

instance Effect WriteOnce where
    type Inv WriteOnce s t = ()
    type Unit WriteOnce = '[]
    type Plus WriteOnce s t = s :++ t

    return x = W (x, Nil)
    (W (a, r)) >>= k = let (W (b, s)) = k a in W (b, r `append` s)
 
put :: a -> WriteOnce (a ': '[]) ()
put x = W ((), Cons x Nil)



