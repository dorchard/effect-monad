{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, GADTs, 
             EmptyDataDecls, UndecidableInstances, RebindableSyntax, OverlappingInstances, 
             DataKinds, TypeOperators, PolyKinds, ConstraintKinds, FlexibleContexts, ScopedTypeVariables  
             #-}

module Control.IxMonad.Reader (Reader(..), ask, merge, (:->)(..), Var(..), Subset, Set(..)) where

import Control.IxMonad
import Control.IxMonad.Helpers.Set
import Control.IxMonad.Helpers.Mapping
import Prelude hiding (Monad(..))
import GHC.TypeLits

import GHC.Prim

data Reader s a = IxR { runReader :: Set s -> a }

instance IxMonad Reader where
    type Inv Reader f g = (IsSet f, IsSet g, Split f g (Union f g))

    type Unit Reader = '[]
    type Plus Reader f g = Union f g

    return x = IxR $ \Empty -> x
    (IxR e) >>= k = IxR $ \fg -> let (f, g) = split fg
                                 in (runReader $ k (e f)) g

ask :: Var k -> Reader '[k :-> v] v
ask Var = IxR $ \(Ext (Var :-> v) Empty) -> v

merge :: (Unionable s t) => (a -> Reader (Union s t) b) -> Reader s (a -> Reader t b)
merge k = IxR $ \s -> \a -> IxR $ \t -> runReader (k a) (union s t)

instance Subset s t => Subeffect Reader s t where
    sub (IxR e) = IxR $ \st -> let s = subset st in e s

