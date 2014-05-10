{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, GADTs, 
             ConstraintKinds, TypeOperators, DataKinds, UndecidableInstances #-}

module Control.IxComonad.Coreader where

import Control.IxComonad
import Control.IxMonad.Helpers.Set
import Control.IxMonad.Helpers.Mapping

-- Indexed reader type

data IxCoreader s a = IxR { runCoreader :: (a, Set s) }

-- Indexed monad instance

instance IxComonad IxCoreader where
    type Inv IxCoreader s t = (Unionable s t, Split s t (Union s t))

    type Unit IxCoreader = '[]
    type Plus IxCoreader s t = Union s t

    extract (IxR (x, Empty)) = x
    extend k (IxR (x, st)) = let (s, t) = split st
                             in IxR (k (IxR (x, t)), s)

instance IxComonadZip IxCoreader where
    type Meet IxCoreader s t    = Union s t
    type CzipInv IxCoreader s t = (Unionable s t)

    czip (IxR (a, s)) (IxR (b, t)) = IxR ((a, b), union s t)

-- 'ask' monadic primitive

ask :: Var v -> IxCoreader '[v :-> a] b -> a
ask _ = \(IxR (_, Ext (Var :-> a) Empty)) -> a

