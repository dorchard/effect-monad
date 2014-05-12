{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators, TypeFamilies, 
             MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, 
             ScopedTypeVariables, PolyKinds, FlexibleContexts, InstanceSigs #-}

module Control.IxMonad.Writer(Writer(..), Symbol, put, (:->), IsSet, Set(..), union, Var(..), 
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
    type Inv Writer s t = (s ~ AsSet s, t ~ AsSet t, Unionable s t)

    type Unit Writer = '[]
    type Plus Writer s t = Union s t

    return x = Writer (x, Empty)
    (Writer (a, w)) >>= k = let Writer (b, w') = k a
                            in  Writer (b, w `union` w')

put :: Var k -> v -> Writer '[k :-> v] ()
put k v = Writer ((), Ext (k :-> v) Empty)

{-- Define the operation for removing duplicates using mappend --}

instance (Monoid u, Nubable ((k :-> u) ': s)) => Nubable ((k :-> u) ': (k :-> u) ': s) where
    nub (Ext (_ :-> u) (Ext (k :-> v) s)) = nub (Ext (k :-> (u `mappend` v)) s)

{-- Sub effecting -}

instance Superset s t => Subeffect Writer s t where
    sub (Writer (a, w)) = Writer (a, (sup w)::(Set t))

class Superset s t where
    sup :: Set s -> Set t

instance Superset '[] '[] where
    sup Empty = Empty

instance (Monoid x, Superset '[] s) => Superset '[] ((k :-> x) ': s) where
    sup Empty = Ext (Var :-> mempty) (sup Empty)

instance Superset s t => Superset ((k :-> v) ': s) ((k :-> v) ': t) where
    sup (Ext x xs) = Ext x (sup xs)

