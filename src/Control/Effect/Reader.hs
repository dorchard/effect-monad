{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts,
             MultiParamTypeClasses, UndecidableInstances, RebindableSyntax,
             DataKinds, TypeOperators, PolyKinds, ConstraintKinds,
             KindSignatures #-}

module Control.Effect.Reader (Reader(..), ask, merge, Mapping(..),
                              Var(..), Submap, Map(..)) where

import Control.Effect
import Data.Type.Map
import Prelude hiding (Monad(..))
import GHC.TypeLits
import GHC.Exts ( Constraint )

{-| Provides a effect-parameterised version of the class reader monad. Effects
   are sets of variable-type pairs, providing an effect system for reader effects. -}

newtype Reader (s :: [Mapping Symbol *]) a = IxR { runReader :: Map s -> a }

instance Effect Reader where
    type Inv Reader f g = (IsMap f, IsMap g, Split f g (Union f g))

    {-| A trivial effect is the empty set -}
    type Unit Reader = '[]
    {-| Effects are combined by set union -}
    type Plus Reader f g = Union f g

    {-| Trivially pure computation has an empty reader environment -}
    return x = IxR $ \Empty -> x
    {-| Composing copmutations splits the reader requirements between the two -}
    (IxR e) >>= k = IxR $ \fg -> let (f, g) = split fg
                                 in (runReader $ k (e f)) g

-- Values of the same type can be combined
type instance Combine v v = v

{-| 'ask' for a variable 'v' of type 'a', raising an effect -}
ask :: Var v -> Reader '[v :-> a] a
ask Var = IxR $ \(Ext Var a Empty) -> a

{-| Provides a way to emulated the ImplicitParams features of GHC/Haskell -}
merge :: (Unionable s t) => (a -> Reader (Union s t) b) -> Reader s (a -> Reader t b)
merge k = IxR $ \s -> \a -> IxR $ \t -> runReader (k a) (union s t)

{-| If 's' is a subset of 't' then, 's' is a subeffect of 't' -}
instance Submap s t => Subeffect Reader s t where
    sub (IxR e) = IxR $ \st -> let s = submap st in e s

{-
{-| Define the operation for removing duplicates using mappend -}
instance (Nubable (e ': s)) => Nubable (e ': e ': s) where
    nub (Ext _ (Ext x xs)) = nub (Ext x xs)
-}
