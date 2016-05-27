{-# LANGUAGE TypeFamilies, GADTs, MultiParamTypeClasses, FlexibleInstances,
             FlexibleContexts, DataKinds, UndecidableInstances, ScopedTypeVariables #-}

module ArrayReader where

import GHC.TypeLits
import Data.Array
import Prelude hiding (Monad(..))

import Control.Effect
import Data.Type.Set

-- Array with a cursor
data CArray (x::[*]) a = MkA (Array Int a, Int)

-- Computations from 'a' to 'b' with an array parameter
data Stencil a (r::[*]) b = Stencil (CArray r a -> b)

-- Get the nth index from the array, relative to the 'cursor'
ix :: (ToValue (IntT x) Int) => IntT x -> Stencil a '[IntT x] a
ix n = Stencil (\(MkA (a, cursor)) -> a ! (cursor + toValue n))

instance Effect (Stencil a) where
    type Inv (Stencil a) s t = ()
    type Plus (Stencil a) s t = Union s t -- append specs
    type Unit (Stencil a)     = '[]       -- empty spec

    (Stencil f) >>= k =
        Stencil (\(MkA a) -> let (Stencil f') = k (f (MkA a))
                                 in f' (MkA a))
    return a = Stencil (\_ -> a)

data Sign n = Pos n | Neg n
data IntT (n :: Sign Nat) = IntT

class ToValue t t' where
    toValue :: t -> t'

instance (KnownNat n) => ToValue (IntT (Pos (n :: Nat))) Int where
    toValue _ = fromInteger $ natVal (Proxy :: (Proxy n))

instance (KnownNat n) => ToValue (IntT (Neg (n :: Nat))) Int where
    toValue _ = - (fromInteger $ natVal (Proxy :: (Proxy n)))

type instance Cmp (IntT (Pos n)) (IntT (Pos m)) = CmpNat n m
type instance Cmp (IntT (Neg n)) (IntT (Neg m)) = CmpNat n m
type instance Cmp (IntT (Pos n)) (IntT (Neg m)) = GT
type instance Cmp (IntT (Neg n)) (IntT (Pos m)) = LT
