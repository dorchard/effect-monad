'Indexed comonads' 

For more details see "Coeffects: Unified static analysis of context-dependence"
by Petricek, Orchard, Mycroft:
http://www.cl.cam.ac.uk/~dao29/publ/coeffects-icalp13.pdf

%if False

> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE ConstraintKinds #-}

> module IxComonad where 

> import GHC.Prim

%endif

> class IxComonad (m :: * -> * -> *) where
>     type Inv m s t :: Constraint -- invariants (i.e. restrict to a subcategory of Hask)
>     type Inv m s t = ()

>     type Unit m 
>     type Plus m s t
>
>     iextract :: c (Unit m) a -> a
>     iextend :: Inv m s t => (c t a -> b) -> c (Plus c s t) a -> c t b
