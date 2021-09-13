{-# LANGUAGE KindSignatures, TypeFamilies, ConstraintKinds, PolyKinds, MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Control.Effect where

import Prelude hiding (Monad(..), Applicative(..))
import GHC.Exts ( Constraint )

class (forall k . Functor (m k)) => EffectApplicative (m :: k -> * -> *) where
   {-| Effect of a trivially effectful computation |-}
   type Unit m :: k
   {-| Cominbing effects of two subcomputations |-}
   type Plus m (f :: k) (g :: k) :: k

   {-| 'Inv' provides a way to give instances of 'Effect' their own constraints for '>>=' -}
   type Inv m (f :: k) (g :: k) :: Constraint
   type Inv m f g = ()

   pure :: a -> m (Unit m) a

   (<*>) :: (Inv m f g) => m f (a -> b) -> m g a -> m (Plus m f g) b

{-| Specifies "parametric effect monads" which are essentially monads but
     annotated by a type-level monoid formed by 'Plus' and 'Unit' -}
class EffectApplicative m => Effect (m :: k -> * -> *) where
   {-| Effect-parameterised version of 'return'. Annotated with the 'Unit m' effect,
    denoting pure compuation -}
   return :: a -> m (Unit m) a
   return = pure

   {-| Effect-parameterise version of '>>=' (bind). Combines
    two effect annotations 'f' and 'g' on its parameter computations into 'Plus' -}

   (>>=) :: (Inv m f g) => m f a -> (a -> m g b) -> m (Plus m f g) b

   (>>) :: (Inv m f g) => m f a -> m g b -> m (Plus m f g) b
   x >> y = x >>= (\_ -> y)

fail = undefined

{-| Specifies subeffecting behaviour -}
class Subeffect (m :: k -> * -> *) f g where
    sub :: m f a -> m g a

liftE2 :: (Effect m, Inv m f g) => m f (a -> b) -> m g a -> m (Plus m f g) b
liftE2 f a = f >>= (<$> a)
