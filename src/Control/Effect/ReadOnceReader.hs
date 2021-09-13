{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}

module Control.Effect.ReadOnceReader (ask,Reader(..),List(..)) where

import Control.Effect
import Control.Effect.Cond
import Control.Effect.Helpers.List
import Prelude    hiding (Monad(..))

{-| Provides a weak reader monad, which can only read an item once. Provides
   an effect system as a list of the items that have been read -}

data Reader (r :: [*]) a = R { runReader :: (List r -> a) }
    deriving (Functor)

instance EffectApplicative Reader where
    type Inv Reader s t = Split s t

    type Unit Reader = '[]
    type Plus Reader s t = s :++ t

    pure x = R $ \Nil -> x
    (<*>) = liftE2

instance Effect Reader where
    (R e) >>= k = R $ \xs -> let (s, t) = split xs
                             in (runReader $ k (e s)) t

{-| 'ask' for a value of type 'a' -}
ask :: Reader '[a] a
ask = R $ \(Cons a Nil) -> a

instance Cond Reader where
    type AltInv Reader s t = Split s t
    type Alt Reader s t = s :++ t

    ifM True (R x) (R y) = R $ \rs -> let (r, s) = split rs
                                          _      = y s
                                      in x r
    ifM False (R x) (R y) = R $ \rs -> let (r, s) = split rs
                                           _      = x r
                                       in y s
