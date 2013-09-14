> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE ConstraintKinds #-}

> module IxMonad where 

> import GHC.Prim

> class IxMonad (m :: * -> * -> *) where
>     type Unit m 
>     type Plus m s t

>     type Inv m s t :: Constraint -- invariants
>     type Inv m s t = ()

>     ireturn :: a -> m (Unit m) a
>     ibind :: Inv m s t => (a -> m t b) -> m s a -> m (Plus m s t) b

> (>:>=) :: (Inv m s t, IxMonad m) => m s a -> (a -> m t b) -> m (Plus m s t) b
> (>:>=) = flip ibind