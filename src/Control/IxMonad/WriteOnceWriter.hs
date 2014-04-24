{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, RebindableSyntax #-}

module Control.IxMonad.WriteOnceWriter (HNil'(..), HCons'(..), put, AppendA(..)) where

import Control.IxMonad
import Data.HList hiding (Monad(..), append)
import Prelude hiding (Monad(..))

instance IxMonad (,) where
    type Inv (,) s t = AppendA s t

    type Unit (,) = HNil'
    type Plus (,) s t = Append s t

    return x = (HNil', x)
    (r, a) >>= k = let (s, b) = k a in (r `append` s, b)
 
put :: a -> (HCons' a HNil', ())
put x = (HCons' x HNil', ())

-- Type-level append

class AppendA s t where
    type Append s t
    append :: s -> t -> Append s t

instance AppendA HNil' t where
    type Append HNil' t = t
    append HNil' t = t

instance AppendA xs ys => AppendA (HCons' x xs) ys where
    type Append (HCons' x xs) ys = HCons' x (Append xs ys)
    append (HCons' x xs) ys = HCons' x (append xs ys)

