{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- This shouldn't be needed since I have functional dependencies
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Effect.Arrow where

import GHC.Exts ( Constraint )

import Control.Effect
import qualified Control.Effect as E

newtype Kleisli m f a b = Kleisli { runKleisli :: a -> m f b }

class E.EffectKind cat k => CategoryEffect (cat :: k -> * -> * -> *) where
  id :: cat (Unit cat) a a
  (>>>) :: Inv cat f g => cat f a b -> cat g b c -> cat (Plus cat f g) a c

class CategoryEffect a => ArrowEffect (a :: k -> * -> * -> *) where
  arr :: (b -> c) -> a (Unit a) b c
  first :: a f b c -> a f (b, d) (c, d)

instance EffectKind m k => EffectKind (Kleisli m) k where
  type Unit (Kleisli m) = Unit m
  type Plus (Kleisli m) f g = Plus m f g
  type Inv (Kleisli m) f g = Inv m f g


instance (EffectKind m k, Effect m) => CategoryEffect (Kleisli m) where
  id = Kleisli E.return
  Kleisli f >>> Kleisli g = Kleisli $ \a -> f a E.>>= g
