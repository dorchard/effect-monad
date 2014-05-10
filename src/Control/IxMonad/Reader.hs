{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, GADTs, 
             EmptyDataDecls, UndecidableInstances, RebindableSyntax, OverlappingInstances, 
             DataKinds, TypeOperators, PolyKinds #-}

module Control.IxMonad.Reader (IxReader(..), ask, (:->)(..), Var(..), Set(..)) where

import Control.IxMonad
import Control.IxMonad.Helpers.Set
import Control.IxMonad.Helpers.Mapping
import Prelude hiding (Monad(..))
import GHC.TypeLits
import Data.Proxy

data IxReader s a = IxR { runReader :: Set s -> a }

instance IxMonad IxReader where
    type Inv IxReader f g = Split f g (Union f g)

    type Unit IxReader = '[]
    type Plus IxReader f g = Union f g

    return x = IxR $ \Empty -> x
    (IxR e) >>= k = IxR $ \fg -> let (f, g) = split fg
                                 in (runReader $ k (e f)) g

ask :: Var (k::Symbol) -> IxReader '[k :-> v] v
ask Var = IxR $ \(Ext (Var :-> v) Empty) -> v

