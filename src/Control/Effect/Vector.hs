{-# LANGUAGE GADTs, TypeFamilies, EmptyDataDecls, UndecidableInstances, MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts #-}

module Control.Effect.Vector where

import Prelude hiding (Monad(..))
import Control.Effect

-- Sized-vector type

data Z
data S n 

data Vector n a where
    Nil :: Vector Z a
    Cons ::a -> Vector n a -> Vector (S n) a

-- Append function which adds indices

type family Add s t
type instance Add Z     m = m
type instance Add (S n) m = S (Add n m)

append :: Vector n a -> Vector m a -> Vector (Add n m) a
append Nil         ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

instance Effect Vector where
    -- Effect monoid is (N, *, 1)
    type Inv Vector s t = ()

    type Unit Vector = S Z

    -- Multiplies indicies
    type Plus Vector Z     m = Z
    type Plus Vector (S n) m = Add m (Plus Vector n m)

    return x = Cons x Nil
    Nil         >>= f  = Nil
    (Cons x xs) >>= f  = append (f x) (xs >>= f)



