{-# LANGUAGE KindSignatures, TypeFamilies, ConstraintKinds, PolyKinds, MultiParamTypeClasses #-}

module Control.Effect where 

import Prelude hiding (Monad(..))
import GHC.Exts ( Constraint )    

{-| Specifies "parametric effect monads" which are essentially monads but
     annotated by a type-level monoid formed by 'Plus' and 'Unit' -}
class Effect (m :: k -> * -> *) where

   {-| Effect of a trivially effectful computation |-}
   type Unit m :: k
   {-| Cominbing effects of two subcomputations |-}
   type Plus m (f :: k) (g :: k) :: k

   {-| 'Inv' provides a way to give instances of 'Effect' their own constraints for '>>=' -}
   type Inv m (f :: k) (g :: k) :: Constraint
   type Inv m f g = ()

   {-| Effect-parameterised version of 'return'. Annotated with the 'Unit m' effect, 
    denoting pure compuation -}
   return :: a -> m (Unit m) a

   {-| Effect-parameterise version of '>>=' (bind). Combines 
    two effect annotations 'f' and 'g' on its parameter computations into 'Plus' -}

   (>>=) :: (Inv m f g) => m f a -> (a -> m g b) -> m (Plus m f g) b

   (>>) :: (Inv m f g) => m f a -> m g b -> m (Plus m f g) b
   x >> y = x >>= (\_ -> y)
  
fail = undefined

{-| Specifies subeffecting behaviour -}
class Subeffect (m :: k -> * -> *) f g where
    sub :: m f a -> m g a

