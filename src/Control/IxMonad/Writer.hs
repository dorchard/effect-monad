{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators, TypeFamilies, 
             MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}

module Control.IxMonad.Writer(Writer(..), Proxy(..), Symbol, put, (:->), Set(..), union) where

import Control.IxMonad 
import Control.IxMonad.Helpers.Set
import Prelude hiding (Monad(..))

data Writer w a = Writer { runWriter :: (a, Set w) } 

instance IxMonad Writer where
    type Inv Writer s t = Unionable s t

    type Unit Writer = '[]
    type Plus Writer s t = Union s t

    return x = Writer (x, Empty)
    (Writer (a, w)) >>= k = let Writer (b, w') = k a
                            in  Writer (b, w `union` w')

put :: Proxy (k :: Symbol) -> v -> Writer '[k :-> v] ()
put k v = Writer ((), Ext k v Empty)

