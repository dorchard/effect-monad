{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, DataKinds,
             TypeOperators #-}

module Control.IxMonad.ReadOnceReader (ask,Reader(..)) where

import Control.IxMonad
import Control.IxMonad.Cond
import Control.IxMonad.Helpers.List
import Prelude    hiding (Monad(..))

data Reader (r :: [*]) a = R { runReader :: (List r -> a) }

instance IxMonad Reader where
    type Inv Reader s t = Split s t

    type Unit Reader = '[]
    type Plus Reader s t = s :++ t

    return x = R $ \Nil -> x
    (R e) >>= k = R $ \xs -> let (s, t) = split xs
                             in (runReader $ k (e s)) t

ask :: Reader ('[a]) a
ask = R $ \(Cons a Nil) -> a


-- Conditionals

instance Cond Reader where
    type AltInv Reader s t = Split s t
    type Alt Reader s t = s :++ t

    ifM True (R x) (R y) = R $ \rs -> let (r, s) = split rs
                                          _      = y s 
                                      in x r
    ifM False (R x) (R y) = R $ \rs -> let (r, s) = split rs
                                           _      = x r 
                                       in y s