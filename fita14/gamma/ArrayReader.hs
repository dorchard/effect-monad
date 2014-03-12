{-# LANGUAGE TypeFamilies, GADTs, MultiParamTypeClasses, FlexibleInstances,
             FlexibleContexts #-}

module ArrayReader where 

import Data.Array
import Prelude hiding (Monad(..)) 
import IxMonad

-- Array with a cursor
data CArray x a = MkA (Array Int a, Int) 

-- Computations from 'a' to 'b' with an array parameter
data ArrayReader a r b = ArrayReader (CArray r a -> b)

-- Get the nth index from the array, relative to the 'cursor'
ix :: IntT x -> ArrayReader a (HCons x HNil) a
ix n = ArrayReader (\(MkA (a, cursor)) -> a ! (cursor + toValue n))

instance IxMonad (ArrayReader a) where
    type Plus (ArrayReader a) s t = Append s t -- append specs
    type Unit (ArrayReader a)     = HNil       -- empty spec

    (ArrayReader f) >>= k = 
        ArrayReader (\(MkA a) -> let (ArrayReader f') = k (f (MkA a))
                                 in f' (MkA a))
    return a = ArrayReader (\_ -> a)


-- Type-level lists

type family Append s t
type instance Append HNil t = t
type instance Append (HCons s ss) t = HCons s (Append ss t)

data HNil
data HCons x xs

data HList t where
    HNil :: HList HNil
    HCons :: x -> HList xs -> HList (HCons x xs)

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
