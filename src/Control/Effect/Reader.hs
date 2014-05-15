{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, 
             UndecidableInstances, RebindableSyntax, DataKinds, TypeOperators, PolyKinds, 
             ConstraintKinds #-}

module Control.Effect.Reader (Reader(..), ask, merge, (:->)(..), Var(..), Subset, Set(..)) where

import Control.Effect
import Control.Effect.Helpers.Set
import Control.Effect.Helpers.Mapping
import Prelude hiding (Monad(..))
import GHC.TypeLits
import GHC.Prim

{- Provides a effect-parameterised version of the class reader monad. Effects
   are sets of variable-type pairs, providing an effect system for reader effects. -}

data Reader s a = IxR { runReader :: Set s -> a }

instance Effect Reader where
    type Inv Reader f g = (IsSet f, IsSet g, Split f g (Union f g))

    {-| A trivial effect is the empty set |-}
    type Unit Reader = '[]
    {-| Effects are combined by set union |-}
    type Plus Reader f g = Union f g

    {-| Trivially pure computation has an empty reader environment |-}
    return x = IxR $ \Empty -> x
    {-| Composing copmutations splits the reader requirements between the two |-}
    (IxR e) >>= k = IxR $ \fg -> let (f, g) = split fg
                                 in (runReader $ k (e f)) g

{-| 'ask' for a variable 'v' of type 'a', raising an effect |-}
ask :: Var v -> Reader '[v :-> a] a
ask Var = IxR $ \(Ext (Var :-> a) Empty) -> a

{-| Provides a way to emulated the ImplicitParams features of GHC/Haskell |-}
merge :: (Unionable s t) => (a -> Reader (Union s t) b) -> Reader s (a -> Reader t b)
merge k = IxR $ \s -> \a -> IxR $ \t -> runReader (k a) (union s t)

{-| If 's' is a subset of 't' then, 's' is a subeffect of 't' |-}
instance Subset s t => Subeffect Reader s t where
    sub (IxR e) = IxR $ \st -> let s = subset st in e s

