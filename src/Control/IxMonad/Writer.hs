{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators, TypeFamilies, 
             MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, 
             ScopedTypeVariables, PolyKinds, FlexibleContexts, InstanceSigs #-}

module Control.IxMonad.Writer(Writer(..), Symbol, put, (:->), Set(..), union, Var(..), 
                              Union, Unionable) where

import Control.IxMonad 
import Control.IxMonad.Helpers.Mapping
import Control.IxMonad.Helpers.Set
import Data.Monoid
import Data.Proxy 
import GHC.TypeLits
import Prelude hiding (Monad(..))

data Writer w a = Writer { runWriter :: (a, Set w) }

{-- Writer effect-parameterised monad -}

instance IxMonad Writer where
    type Inv Writer s t = Unionable s t

    type Unit Writer = '[]
    type Plus Writer s t = Union s t

    return x = Writer (x, Empty)
    (Writer (a, w)) >>= k = let Writer (b, w') = k a
                            in  Writer (b, w `union` w')

put :: Var k -> v -> Writer '[k :-> v] ()
put k v = Writer ((), Ext (k :-> v) Empty)

{-- Define the operation for removing duplicates using mappend --}

instance (Monoid u, Nubable ((k :-> u) ': s) s') => Nubable ((k :-> u) ': (k :-> u) ': s) s' where
    nub (Ext (_ :-> u) (Ext (k :-> v) s)) = nub (Ext (k :-> (u `mappend` v)) s)

