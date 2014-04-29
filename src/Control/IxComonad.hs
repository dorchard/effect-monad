{- 'Indexed comonads' 

For more details see "Coeffects: Unified static analysis of context-dependence"
by Petricek, Orchard, Mycroft:
http://www.cl.cam.ac.uk/~dao29/publ/coeffects-icalp13.pdf

-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds, PolyKinds, ScopedTypeVariables, DataKinds #-}

module Control.IxComonad where 

import GHC.Prim

class IxComonad (c :: k -> * -> *) where
    type Inv c (s :: k) (t :: k) :: Constraint -- invariants (i.e. restrict to a subcategory of Hask)
    type Inv c s t = ()

    type Unit c :: k 
    type Plus c (s :: k) (t :: k) :: k

    extract :: c (Unit c) a -> a
    extend :: Inv c s t => (c t a -> b) -> c (Plus c s t) a -> c s b

class IxCZip (c :: k -> * -> *) where
    type Meet c (s :: k) (t :: k) :: k
    type CzipInv c (s :: k) (t :: k) :: Constraint
    iczip :: CzipInv c s t => c s a -> c t b -> c (Meet c s t) (a, b)