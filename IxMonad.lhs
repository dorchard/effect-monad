'Indexed monads' 

Dual to 'indexed comonads' 

%if False

> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE ConstraintKinds #-}

> module IxMonad where 

> import GHC.Prim

%endif

> class IxMonad (m :: * -> * -> *) where
>     type Inv m s t :: Constraint -- invariants (i.e. restrict to a subcategory of Hask)
>     type Inv m s t = ()

>     type Unit m 
>     type Plus m s t
>
>     ireturn :: a -> m (Unit m) a
>     ibind :: Inv m s t => (a -> m t b) -> m s a -> m (Plus m s t) b

We provide an infix version of |ibind| that resembles the usual |>>=| (bind)
operation of a monad in Haskell:

> (>>=:) :: (Inv m s t, IxMonad m) => m s a -> (a -> m t b) -> m (Plus m s t) b
> (>>=:) = flip ibind

