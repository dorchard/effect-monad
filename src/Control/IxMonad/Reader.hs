{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, GADTs, 
             EmptyDataDecls, UndecidableInstances, RebindableSyntax, OverlappingInstances, 
             DataKinds, TypeOperators, PolyKinds, ConstraintKinds, FlexibleContexts, ScopedTypeVariables  
             #-}

module Control.IxMonad.Reader (IxReader(..), ask, merge, (:->)(..), Var(..), Sub, Set(..)) where

import Control.IxMonad
import Control.IxMonad.Helpers.Set hiding (sub)
import qualified Control.IxMonad.Helpers.Set as Set
import Control.IxMonad.Helpers.Mapping
import Prelude hiding (Monad(..))
import GHC.TypeLits

import GHC.Prim

data IxReader s a = IxR { runReader :: Set s -> a }

instance IxMonad IxReader where
    type Inv IxReader f g = (f ~ AsSet f, g ~ AsSet g, Split f g (Union f g))

    type Unit IxReader = '[]
    type Plus IxReader f g = Union f g

    return x = IxR $ \Empty -> x
    (IxR e) >>= k = IxR $ \fg -> let (f, g) = split fg
                                 in (runReader $ k (e f)) g

ask :: Var k -> IxReader '[k :-> v] v
ask Var = IxR $ \(Ext (Var :-> v) Empty) -> v

merge :: (Unionable s t) => (a -> IxReader (Union s t) b) -> IxReader s (a -> IxReader t b)
merge k = IxR $ \s -> \a -> IxR $ \t -> runReader (k a) (union s t)

instance Subeffect IxReader where
    type SubInv IxReader s t = Sub s t
    sub (IxR e) = IxR $ \st -> let s = Set.sub st in e s

