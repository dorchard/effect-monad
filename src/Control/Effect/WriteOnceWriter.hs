{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances,
             TypeOperators, DataKinds, KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}

module Control.Effect.WriteOnceWriter (put, WriteOnce(..)) where

import Control.Effect
import Control.Effect.Helpers.List
import Prelude hiding (Monad(..))

{-| Provides a kind of writer monad, which can only write an item once
   (no accumulation), an effect system as a list of the items that have been written -}

data WriteOnce (w :: [*]) a = W { runWriteOnce :: (a, List w) }
    deriving (Functor)

instance EffectApplicative WriteOnce where
    type Inv WriteOnce s t = ()

    {-| Pure effect is the empty list -}
    type Unit WriteOnce = '[]

    {-| Combine effects by appending effect information -}
    type Plus WriteOnce s t = s :++ t

    pure x = W (x, Nil)
    (<*>) = liftE2

instance Effect WriteOnce where
    (W (a, r)) >>= k = let (W (b, s)) = k a in W (b, r `append` s)

{-| Write a value of type 'a' -}
put :: a -> WriteOnce '[a] ()
put x = W ((), Cons x Nil)
