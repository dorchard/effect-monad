{-# LANGUAGE KindSignatures, TypeFamilies, ConstraintKinds, PolyKinds, DataKinds #-}

module Control.Effect.CatIndexed where

import Control.Effect

newtype M (i :: Morph * *) a = M a

data Morph a b = M a b | Id

instance Effect (M :: ((Morph * *) -> * -> *)) where
    type Unit M = Id
    type Plus M (M a b) (M c d) = M a d
    type Plus M Id (M a b) = M a b
    type Plus M (M a b) Id = M a b
    type Inv  M (M a b) (M c d) = c ~ d

    return a = M a
    (M x) >>= k = let M y = k x in M y
