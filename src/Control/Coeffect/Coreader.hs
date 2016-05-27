{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, GADTs,
             ConstraintKinds, TypeOperators, DataKinds, UndecidableInstances #-}

module Control.Coeffect.Coreader where

import Control.Coeffect
import Data.Type.Map
import GHC.TypeLits

{-| Provides 'reader monad'-like behaviour but as a comonad, using an indexed
    version of the product comonad -}
data IxCoreader (s :: [Mapping Symbol *]) a = IxR { runCoreader :: (a, Map s) }

instance Coeffect IxCoreader where
    type Inv IxCoreader s t = (Unionable s t, Split s t (Union s t))

    type Unit IxCoreader = '[]
    type Plus IxCoreader s t = Union s t

    extract (IxR (x, Empty)) = x
    extend k (IxR (x, st)) = let (s, t) = split st
                             in IxR (k (IxR (x, t)), s)

instance CoeffectZip IxCoreader where
    type Meet IxCoreader s t    = Union s t
    type CzipInv IxCoreader s t = (Unionable s t)

    czip (IxR (a, s)) (IxR (b, t)) = IxR ((a, b), union s t)


{-| 'ask' for the value of variable 'v', e.g., 'ask (Var::(Var "x"))' -}
ask :: Var v -> IxCoreader '[v :-> a] b -> a
ask _ = \(IxR (_, Ext Var a Empty)) -> a
