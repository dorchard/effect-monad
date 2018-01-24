{-# LANGUAGE KindSignatures, TypeFamilies, ConstraintKinds, PolyKinds, DataKinds #-}

module Control.Effect.ParameterisedAsGraded where

import Control.Effect

{-| Implements Bob Atkey's 'parametric monads',
    and also the Control.Monad.Indexed package, by emulating
    indexing by morphisms -}

{-| Data type of morphisms -}
newtype T (i :: Morph * *) a = T a

{-| Data type denoting either a morphisms with source and target types, or identity -}
data Morph a b = M a b | Id

instance Effect (T :: ((Morph * *) -> * -> *)) where
    type Unit T = Id
    type Plus T (M a b) (M c d) = M a d
    type Plus T Id (M a b) = M a b
    type Plus T (M a b) Id = M a b
    type Inv  T (M a b) (M c d) = c ~ d

    return a = T a
    (T x) >>= k = let T y = k x in T y