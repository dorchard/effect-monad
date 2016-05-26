{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds, PolyKinds, ScopedTypeVariables, DataKinds #-}

module Control.Coeffect where 

import GHC.Exts ( Constraint )

{- Coeffect parameterised comonad

Also called "indexed comonads". 
For more details see "Coeffects: Unified static analysis of context-dependence"
by Petricek, Orchard, Mycroft: http://www.cl.cam.ac.uk/~dao29/publ/coeffects-icalp13.pdf

-}

{-| Specifies "parametric coeffect comonads" which are essentially comonads but
     annotated by a type-level monoid formed by 'Plus' and 'Unit' -}
class Coeffect (c :: k -> * -> *) where
    type Inv c (s :: k) (t :: k) :: Constraint
    type Inv c s t = ()

    type Unit c :: k 
    type Plus c (s :: k) (t :: k) :: k

    {-| Coeffect-parameterised version of 'extract', 
         annotated with the 'Unit m' effect, denoting pure contexts -}
    extract :: c (Unit c) a -> a

    {-| Coeffect-parameterise version of 'extend'.
        The two coeffec annotations 's' and 't' on its parameter computations
          get combined in the parameter computation by 'Plus' -}
    extend :: Inv c s t => (c t a -> b) -> c (Plus c s t) a -> c s b

{-| Zips two coeffecting computations together -}
class CoeffectZip (c :: k -> * -> *) where
    type Meet c (s :: k) (t :: k) :: k
    type CzipInv c (s :: k) (t :: k) :: Constraint
    czip :: CzipInv c s t => c s a -> c t b -> c (Meet c s t) (a, b)

{-| Specifies sub-coeffecting behaviour -}
class Subcoeffect (c :: k -> * -> *) s t where
    subco :: c s a -> c t a