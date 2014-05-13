{-# LANGUAGE TypeFamilies, ConstraintKinds, PolyKinds #-}

module Control.Effect.Cond where

import GHC.Prim

class Cond (m :: k -> * -> *) where
    type AltInv m (s :: k) (t :: k) :: Constraint
    type AltInv m s t = ()

    type Alt m (s :: k) (t :: k) :: k 
    ifM :: AltInv m s t => Bool -> m s a -> m t a -> m (Alt m s t) a

-- Related to indexed 'applicative' functors/idioms
--            which are indexed monoidal functors
