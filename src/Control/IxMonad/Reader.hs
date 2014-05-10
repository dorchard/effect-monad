{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, GADTs, 
             EmptyDataDecls, UndecidableInstances, RebindableSyntax, OverlappingInstances, 
             DataKinds, TypeOperators, PolyKinds, ConstraintKinds, FlexibleContexts, AllowAmbiguousTypes 
             #-}

module Control.IxMonad.Reader (IxReader(..), ask, (:->)(..), Var(..), Set(..),Subeffect(..),Proxy(..)) where

import Control.IxMonad
import Control.IxMonad.Helpers.Set
import Control.IxMonad.Helpers.Mapping
import Prelude hiding (Monad(..))
import GHC.TypeLits
import Data.Proxy

import GHC.Prim

data IxReader s a = IxR { runReader :: Set s -> a }

instance IxMonad IxReader where
    type Inv IxReader f g = Split f g (Union f g)

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
    type Join IxReader s t = Union s t
    type SubInv IxReader s t = Split s t (Union s t)
    subEffect p (IxR e) = IxR $ \st -> let (s, t) = split st 
                                           _ = ReflP p t 
                                       in e s

-- Equality proof between a set and a proxy
data EqT a b where
    ReflP :: Proxy t -> Set t -> EqT t t