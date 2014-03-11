{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

module Cond where

import GHC.Prim

class Cond m where
    type AltInv m s t :: Constraint
    type Alt m s t 
    ifM :: AltInv m s t => Bool -> m s a -> m t a -> m (Alt m s t) a

-- Related to indexed 'applicative' functors/idioms
--            which are indexed monoidal functors
