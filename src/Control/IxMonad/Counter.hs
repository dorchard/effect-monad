{-# LANGUAGE TypeFamilies, EmptyDataDecls, TypeOperators #-}

module Control.IxMonad.Counter(Z, S, Counter, tick, (:+)) where

import Control.IxMonad
import Prelude hiding (Monad(..))

data Z
data S n

data Counter n a = Counter { forget :: a }

type family n :+ m 
type instance n :+ Z     = n
type instance n :+ (S m) = S (n :+ m)

instance IxMonad Counter where
    type Unit Counter = Z
    type Plus Counter n m = n :+ m

    return a = Counter a
    (Counter a) >>= k = Counter . forget $ k a

tick :: a -> Counter (S Z) a
tick x = Counter x
