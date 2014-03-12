{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}

module Control.IxMonad.Reader

import Control.IxMonad
import Data.HList hiding (Monad(..))
import Prelude    hiding (Monad(..))

instance IxMonad (->) where
    type Inv (->) s t = Split s t

    type Unit (->) = HNil
    type Plus (->) s t = Append s t

    return x = \HNil -> x
    e >>= k = \xs -> let (s, t) = split xs
                     in (k (e s)) t

ask :: (HCons a HNil) -> a
ask = \(HCons a HNil) -> a

-- Type-level append, and dual operations for split

class Split s t where
    type Append s t
    split :: Append s t -> (s, t)

instance Split HNil t where
    type Append HNil t = t
    split t = (HNil, t)

instance Split xs ys => Split (HCons x xs) ys where
    type Append (HCons x xs) ys = HCons x (Append xs ys)
    split (HCons x xs) = let (xs', ys') = split xs
                         in (HCons x xs', ys')
