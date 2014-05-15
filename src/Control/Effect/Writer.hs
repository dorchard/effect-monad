{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators, TypeFamilies, 
             MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, 
             ScopedTypeVariables, PolyKinds, FlexibleContexts #-}

module Control.Effect.Writer(Writer(..), Symbol, put, (:->), IsSet, Set(..), union, Var(..), 
                              Union, Unionable) where

import Control.Effect 
import Control.Effect.Helpers.Mapping
import Control.Effect.Helpers.Set
import Data.Monoid
import Data.Proxy 
import GHC.TypeLits
import Prelude hiding (Monad(..))

{- Provides an effect-parameterised version of the writer monad. Effects
   are sets of variable-type pairs, providing an effect system for writer effects. -}

data Writer w a = Writer { runWriter :: (a, Set w) }

instance Effect Writer where
    type Inv Writer s t = (IsSet s, IsSet t, Unionable s t)

    {-| A trivial effect is the empty set |-}
    type Unit Writer = '[]
    {-| Effects are combined by set union |-}
    type Plus Writer s t = Union s t

    {-| Trivially pure computation produces an empty state |-}
    return x = Writer (x, Empty)
    {-| Composing copmutations takes the union of the writer states, using the monoid 
        operation to combine writes to the same variable |-}
    (Writer (a, w)) >>= k = let Writer (b, w') = k a
                            in  Writer (b, w `union` w')

{-| Write to variable 'v' with value of type 'a' |-}
put :: Var v -> a -> Writer '[v :-> a] ()
put v a = Writer ((), Ext (v :-> a) Empty)

{-| Define the operation for removing duplicates using mappend |-}
instance (Monoid u, Nubable ((k :-> u) ': s)) => Nubable ((k :-> u) ': (k :-> u) ': s) where
    nub (Ext (_ :-> u) (Ext (k :-> v) s)) = nub (Ext (k :-> (u `mappend` v)) s)

{- Sub effecting for the parametric effect monad -}
instance Superset s t => Subeffect Writer s t where
    sub (Writer (a, w)) = Writer (a, (superset w)::(Set t))

{-| Computes supersets of sets of variable-type mappings, using the 'mempty' operation  |-}
class Superset s t where
    superset :: Set s -> Set t

instance Superset '[] '[] where
    superset Empty = Empty

instance (Monoid x, Superset '[] s) => Superset '[] ((k :-> x) ': s) where
    superset Empty = Ext (Var :-> mempty) (superset Empty)

instance Superset s t => Superset ((k :-> v) ': s) ((k :-> v) ': t) where
    superset (Ext x xs) = Ext x (superset xs)
