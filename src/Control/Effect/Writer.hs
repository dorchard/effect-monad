{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators, TypeFamilies,
             MultiParamTypeClasses, FlexibleInstances, UndecidableInstances,
             ScopedTypeVariables, PolyKinds, FlexibleContexts #-}

module Control.Effect.Writer(Writer(..), Symbol, put, Mapping(..),
                             IsMap, Map(..), union, Var(..),
                             Union, Unionable) where

import Control.Effect
import Data.Type.Map
import Data.Monoid
import GHC.TypeLits
import Prelude hiding (Monad(..))

{-| Provides an effect-parameterised version of the writer monad. Effects
   are maps of variable-type pairs, providing an effect system for writer effects. -}

data Writer (w :: [Mapping Symbol *]) a = Writer { runWriter :: (a, Map w) }

instance Effect Writer where
    type Inv Writer s t = (IsMap s, IsMap t, Unionable s t)

    {-| A trivial effect is the empty map -}
    type Unit Writer = '[]
    {-| Effects are combined by map union -}
    type Plus Writer s t = Union s t

    {-| Trivially pure computation produces an empty state -}
    return x = Writer (x, Empty)
    {-| Composing copmutations takes the union of the writer states, using the monoid
        operation to combine writes to the same variable -}
    (Writer (a, w)) >>= k = let Writer (b, w') = k a
                            in  Writer (b, w `union` w')

{-| Write to variable 'v' with value of type 'a' -}
put :: Var v -> a -> Writer '[v :-> a] ()
put v a = Writer ((), Ext v a Empty)

-- Values of the same type can be combined
type instance Combine v v = v

{-| Define the operation for removing duplicates using mappend -}
instance (Monoid u, Nubable ((k :-> u) ': s)) => Nubable ((k :-> u) ': (k :-> u) ': s) where
    nub (Ext _ u (Ext k v s)) = nub (Ext k (u `mappend` v) s)

{- Sub effecting for the parametric effect monad -}
instance Supermap s t => Subeffect Writer s t where
    sub (Writer (a, w)) = Writer (a, (supermap w)::(Map t))

{-| Computes supermaps of maps of variable-type mappings, using the 'mempty' operation  -}
class Supermap s t where
    supermap :: Map s -> Map t

instance Supermap '[] '[] where
    supermap Empty = Empty

instance (Monoid x, Supermap '[] s) => Supermap '[] ((k :-> x) ': s) where
    supermap Empty = Ext Var mempty (supermap Empty)

instance Supermap s t => Supermap ((k :-> v) ': s) ((k :-> v) ': t) where
    supermap (Ext k x xs) = Ext k x (supermap xs)
