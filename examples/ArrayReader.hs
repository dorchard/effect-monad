{-# LANGUAGE TypeFamilies, GADTs, MultiParamTypeClasses, FlexibleInstances,
             FlexibleContexts, DataKinds, UndecidableInstances #-}

module ArrayReader where 

import GHC.TypeLits hiding (Nat)
import Data.Array
import Prelude hiding (Monad(..)) 
import Control.Effect
import Control.Effect.Helpers.Set

-- Array with a cursor
data CArray (x::[*]) a = MkA (Array Int a, Int) 

-- Computations from 'a' to 'b' with an array parameter
data ArrayReader a (r::[*]) b = ArrayReader (CArray r a -> b)

-- Get the nth index from the array, relative to the 'cursor'
ix :: IntT x -> ArrayReader a '[x] a
ix n = ArrayReader (\(MkA (a, cursor)) -> a ! (cursor + toValue n))

instance Effect (ArrayReader a) where
    type Inv (ArrayReader a) s t = ()
    type Plus (ArrayReader a) s t = Union s t -- append specs
    type Unit (ArrayReader a)     = '[]       -- empty spec

    (ArrayReader f) >>= k = 
        ArrayReader (\(MkA a) -> let (ArrayReader f') = k (f (MkA a))
                                 in f' (MkA a))
    return a = ArrayReader (\_ -> a)


-- Type-level integers
data Z
data S n 

data Nat n where
   Z :: Nat Z
   S :: Nat n -> Nat (S n)

natToInt :: Nat n -> Int
natToInt Z = 0
natToInt (S n) = 1 + natToInt n

data Neg n

data IntT n where
   Neg :: Nat (S n) -> IntT (Neg (S n))
   Pos :: Nat n -> IntT n

-- Note that zero is "positive"

intTtoInt :: IntT n -> Int
intTtoInt (Pos n) = natToInt n
intTtoInt (Neg n) = - natToInt n

class ToValue t t' where
    toValue :: t -> t'

instance ToValue (Nat n) Int where
    toValue n = natToInt n

instance ToValue (IntT n) Int where
    toValue n = intTtoInt n

instance (ToValue m Int, ToValue n Int) => ToValue (m, n) (Int, Int) where
    toValue (m, n) = (toValue m, toValue n)

type instance Min Z Z = Z 
type instance Min Z (S n) = Z
type instance Min (S n) Z = Z 
type instance Min (S n) (S m) = S (Min n m)
type instance Min Z (Neg m) = Neg m
type instance Min (Neg m) Z  = Neg m
type instance Min (S m) (Neg n) = Neg n
type instance Min (Neg m) (S n) = Neg m
type instance Min (Neg (S m)) (Neg (S n)) = Neg (S (Min m n))

type instance Max Z Z = Z
type instance Max Z (S n) = (S n)
type instance Max (S n) Z = (S n)
type instance Max (S n) (S m) = S (Max n m)
type instance Max Z (Neg m) = Z
type instance Max (Neg m) Z  = Z
type instance Max (S m) (Neg n) = (S m)
type instance Max (Neg m) (S n) = S n
type instance Max (Neg (S m)) (Neg (S n)) = Neg (S (Max m n))
