{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, GADTs, 
             EmptyDataDecls, UndecidableInstances, RebindableSyntax, OverlappingInstances #-}

module Control.IxMonad.SetReader where

import Control.IxMonad
import Prelude hiding (Monad(..))

-- Type-level list

data Nil
data Cons x xs

data List n where
    Nil :: List Nil
    Cons :: x -> List xs -> List (Cons x xs)

-- Type-level set union
--    implemented using lists, with a canonical ordering and duplicates removed
type family Union s t where Union s t = RemDup (Bubble (Append' s t))

-- Type-level list append
type family Append' s t where
       Append' Nil t = t
       Append' (Cons x xs) ys = Cons x (Append' xs ys)

-- Type-level natural numbers

data Z 
data S n

data Nat n where
    Z :: Nat Z
    S :: Nat n -> Nat (S n)

-- Remove duplicates from a type-level list
type family RemDup t where
            RemDup Nil                  = Nil
            RemDup (Cons a  Nil)        = Cons a Nil
            RemDup (Cons a (Cons a as)) = Cons a (RemDup as)
            RemDup (Cons a (Cons b as)) = Cons a (Cons b (RemDup as))

-- Type-level bubble sort on list
type family Bubble l where
            Bubble Nil                 = Nil
            Bubble (Cons a Nil)        = Cons a Nil
            Bubble (Cons a (Cons b c)) = Cons (Min a b) (Bubble (Cons (Max a b) c))

-- Return the minimum or maximum of two types which consistitue key-value pairs
type family Min n m where 
    Min (Nat n, t) (Nat m, t') = SortLeft' n m (Nat n, t) (Nat m, t')
type family Max n m where 
    Max (Nat n, t) (Nat m, t') = SortLeft' m n (Nat n, t) (Nat m, t')

type family SortLeft' n m p q 
type instance SortLeft' Z Z         p q = p
type instance SortLeft' Z (S m)     p q = p
type instance SortLeft' (S m) Z     p q = q
type instance SortLeft' (S m) (S n) p q = SortLeft' m n p q

-- Indexed reader type

data IxReader s a = IxR { unIxR :: List s -> a }

-- Indexed monad instance

instance IxMonad IxReader where
    type Inv IxReader s t = Split s t (Union s t)

    type Unit IxReader = Nil
    type Plus IxReader s t = Union s t

    return x = IxR $ \Nil -> x
    (IxR e) >>= k = IxR $ \xs -> let (s, t) = split xs
                                 in (unIxR $ k (e s)) t

-- 'ask' monadic primitive

ask :: Nat t -> IxReader (Cons (Nat t, a) Nil) a
ask _ = IxR $ \(Cons (t, a) Nil) -> a

-- Split operation (with type level version)

class Split s t z where
   split :: List z -> (List s, List t)

instance Split Nil Nil Nil where
   split Nil = (Nil, Nil) 

instance Split (Cons x xs) Nil (Cons x xs) where
    split t = (t, Nil)

instance Split Nil (Cons x xs) (Cons x xs) where
   split t = (Nil, t)

instance Split (Cons x Nil) (Cons x Nil) (Cons x Nil) where
   split (Cons x Nil) = (Cons x Nil, Cons x Nil)

instance Split xs ys zs => Split (Cons x xs) (Cons x ys) (Cons x zs) where
   split (Cons x zs) = let (xs', ys') = split zs
                        in (Cons x xs', Cons x ys')

instance (Split xs ys zs) => Split (Cons (r, x) xs) (Cons (s, y) ys) (Cons (r, x) (Cons (s, y) zs)) where
   split (Cons x (Cons y zs)) = let (xs', ys') = split zs
                                 in (Cons x xs', Cons y ys')

instance (Split xs ys zs) => Split (Cons (r, x) xs) (Cons (s, y) ys) (Cons (s, y) (Cons (r, x) zs)) where
   split (Cons x (Cons y zs)) = let (xs', ys') = split zs
                                 in (Cons y xs', Cons x ys')


{- Examples -}

foo :: IxReader (Cons (Nat Z, a) Nil) a
foo = do x <- ask Z
         return x
      
-- unIxR foo (Cons (Z, 42) Nil)

foo2 :: IxReader (Cons (Nat Z, a) (Cons (Nat (S Z), [a]) Nil)) [a]
foo2 = do x <- ask Z
          xs <- ask (S Z)
          return (x : xs) 

-- unIxR foo2 (Cons (Z, 1) (Cons (S Z, [2,3]) Nil))
        
foo3 :: IxReader (Cons (Nat Z, a) (Cons (Nat (S Z), [a]) Nil)) [a]
foo3 = do xs <- ask (S Z)
          x <- ask Z
          return (x : xs) 
      
-- unIxR foo3 (Cons (Z, 1) (Cons (S Z, [2,3]) Nil))

foo4 :: Num a => IxReader (Cons (Nat Z, a) Nil) a
foo4 = do x <- ask Z
          y <- ask Z
          return (x + y)

-- unIxR foo4 (Cons (Z, 42) Nil)
     
foo5 :: IxReader (Cons (Nat Z, a) (Cons (Nat (S Z), [a]) Nil)) [a]
foo5 = do xs <- ask (S Z)
          x <- ask Z
          y <- ask Z
          return (x : (y : xs))

-- unIxR foo5 (Cons (Z, 1) (Cons (S Z, [2,3]) Nil))

foo6 :: IxReader (Cons (Nat Z, a) (Cons (Nat (S Z), [a]) Nil)) [a]
foo6 = do x <- ask Z
          xs <- ask (S Z)
          y <- ask Z
          return (x : (y : xs))

-- unIxR foo6 (Cons (Z, 1) (Cons (S Z, [2,3]) Nil))