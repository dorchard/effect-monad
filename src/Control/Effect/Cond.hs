{-# LANGUAGE TypeFamilies, ConstraintKinds, PolyKinds #-}

module Control.Effect.Cond where

import GHC.Exts ( Constraint )

{-| Provides a conditional using an 'alternation' operation, as opposed to using
   'Subeffect' -}

class Cond (m :: k -> * -> *) where
    type AltInv m (s :: k) (t :: k) :: Constraint
    type AltInv m s t = ()

    {-| Type family for describing how to combine effects of the two
        branches of an if -}
    type Alt m (s :: k) (t :: k) :: k 

    {-| Conditional on effectful operations -}
    ifM :: AltInv m s t => Bool -> m s a -> m t a -> m (Alt m s t) a

-- Related to indexed 'applicative' functors/idioms
--            which are indexed monoidal functors
