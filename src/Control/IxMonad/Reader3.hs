{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, GADTs, 
             EmptyDataDecls, UndecidableInstances, RebindableSyntax, FunctionalDependencies, OverlappingInstances #-}

module Control.IxMonad.Reader3 (Nil, Cons, List(..)) where -- , ask, Split(..)) where

import Control.IxMonad
import Control.IxMonad.Cond
import Prelude    hiding (Monad(..))

data Nil
data Cons x xs

data List n where
    Nil :: List Nil
    Cons :: x -> List xs -> List (Cons x xs)

type family Append s t where Append s t = RemDup (Bubble (Append' s t))

data Z
data S n 

data Nat n where
    Z :: Nat Z
    S :: Nat n -> Nat (S n)

data EqT a b where
    Refl :: EqT a a

instance Show (EqT a b) where
    show Refl = "Refl"

type Named n a = (n, a)

-- type instance Bubble (Cons a (Cons b c)) 

type family RemDup t where
         RemDup Nil                  = Nil
         RemDup (Cons a  Nil)        = Cons a Nil
         RemDup (Cons a (Cons a as)) = Cons a (RemDup as)
         RemDup (Cons a (Cons b as)) = Cons a (Cons b (RemDup as))

type family Bubble l where
               Bubble Nil                 = Nil
               Bubble (Cons a Nil)        = Cons a Nil
               Bubble (Cons a (Cons a c)) = Cons a (Bubble c)
               Bubble (Cons a (Cons b c)) = Cons (SortLeft a b)
                                              (Bubble (Cons (SortRight a b) c))
type SortLeft n m = SortLeft' n m n m  
type SortRight n m = SortRight' n m n m  

type family SortLeft' n m p q 
type instance    SortLeft' (Z, t) (Z, t')     p q = p
type instance    SortLeft' (Z, t) (S m, t')   p q = p
type instance    SortLeft' (S m, t) (Z, t')   p q = q
type instance    SortLeft' (S m, t) (S n, t') p q = SortLeft' (m, t) (n, t') p q

type family SortRight' n m p q 
type instance    SortRight' (Z, t) (Z, t')     p q = q
type instance    SortRight' (Z, t) (S m, t')   p q = q
type instance    SortRight' (S m, t) (Z, t')   p q = p
type instance    SortRight' (S m, t) (S n, t') p q = SortRight' (m, t) (n, t') p q

instance IxMonad (->) where
    type Inv (->) (List s) (List t) = SplitZ s t (Append s t)

    type Unit (->) = List Nil
    type Plus (->) (List s) (List t) = List (Append s t)

    return x = \Nil -> x
    e >>= k = \xs -> let (s, t) = splitZ xs
                     in (k (e s)) t


class LT s t where
instance LT Z (S n) where
instance LT n m => LT (S n) (S m) where

ask :: Nat t -> List (Cons (t, a) Nil) -> a
ask _ (Cons (t, a) Nil) = a

-- Type-level append, and dual operations for split


class Split s t where
    type Append' s t
    split :: List (Append' s t) -> (List s, List t)

instance Split  Nil t where
    type Append' Nil t = t
    split t = (Nil, t)

instance Split xs ys => Split (Cons x xs) ys where
    type Append' (Cons x xs) ys = Cons x (Append' xs ys)
    split (Cons x xs) = let (xs', ys') = split xs
                         in (Cons x xs', ys')


{- Getting there I think...

class (r ~ (RemDup (Bubble (Append' s t)))) => SplitA s t r where
   splitA :: List r -> (List (RemDup (Bubble s)), List (RemDup (Bubble t)))

instance (q ~ (RemDup (Bubble s))) => SplitA Nil s q where
   splitA t = (Nil, t)

instance SplitA (Cons x Nil) (Cons x Nil) (Cons x Nil) where
   splitA (Cons x Nil) = (Cons x Nil, Cons x Nil)

instance (LT r s, zs ~ (RemDup (Bubble (Append' xs ys))), SplitA xs ys zs) => 
          SplitA (Cons (r, x) xs) (Cons (s, y) ys) (Cons (r, x) (Cons (s, y) zs)) where
    splitA (Cons x (Cons y zs)) = let (xs', ys') = splitA zs
                                  in (Cons x xs', Cons y ys') -}

{- bluergh
class (z ~ RemDup (Bubble (Append' s t))) => SplitA s t z | s t -> z where
   splitA :: List z -> (List (RemDup (Bubble s)), List (RemDup (Bubble t)))

instance (RemDup (Bubble s) ~ q) => SplitA Nil s q where
   splitA t = (Nil, t)

instance SplitA (Cons x Nil) (Cons x Nil) (Cons x Nil) where
   splitA (Cons x Nil) = (Cons x Nil, Cons x Nil)

instance (RemDup (Bubble (Cons x (Append' xs (Cons x ys)))) ~ (Cons x zs), 
           SplitA xs ys zs) => SplitA (Cons x xs) (Cons x ys) (Cons x zs) where
   splitA (Cons x zs) = let (xs', ys') = splitA zs
                        in (Cons x xs', Cons x ys')
-}

class SplitZ s t z where
   splitZ :: List z -> (List s, List t)

instance SplitZ Nil Nil Nil where
   splitZ Nil = (Nil, Nil) 

instance SplitZ (Cons x xs) Nil (Cons x xs) where
    splitZ t = (t, Nil)

instance SplitZ Nil (Cons x xs) (Cons x xs) where
   splitZ t = (Nil, t)

instance SplitZ (Cons x Nil) (Cons x Nil) (Cons x Nil) where
   splitZ (Cons x Nil) = (Cons x Nil, Cons x Nil)

instance SplitZ xs ys zs => SplitZ (Cons x xs) (Cons x ys) (Cons x zs) where
   splitZ (Cons x zs) = let (xs', ys') = splitZ zs
                        in (Cons x xs', Cons x ys')

instance (LT r s, SplitZ xs ys zs) => SplitZ (Cons (r, x) xs) (Cons (s, y) ys) (Cons (r, x) (Cons (s, y) zs)) where
   splitZ (Cons x (Cons y zs)) = let (xs', ys') = splitZ zs
                                 in (Cons x xs', Cons y ys')

{-
instance (LT r s, SplitA xs ys) => 
          SplitA (Cons (r, x) xs) (Cons (s, y) ys) where
    splitA (Cons x (Cons y zs)) = let (xs', ys') = splitA zs
                                  in (Cons x xs', Cons y ys')
  
-}                        

--instance SplitA (Cons x (Cons y Nil)) (Cons x (Cons z Nil)) where
--    splitA (Cons x (Cons y (Cons z Nil))) = (Cons x (Cons y Nil), Cons x (Cons z Nil))

{-
instance SplitA xs ys => SplitA (Cons x xs) (Cons x ys) where
   splitA (Cons x xs) = let (xs', ys') = splitA xs
                        in (Cons x xs', Cons x ys')
-}

{-
instance SplitA  xs ys => SplitA (Cons x xs) ys where
   splitA (Cons x xs) = let (xs', ys') = splitA xs
                        in (Cons x xs', ys')
-}

foo :: List (Cons (Z, a) Nil) -> a
foo = do x <- ask Z
         return x
      

foo2 :: List (Cons (Z, a) (Cons (S Z, [a]) Nil)) -> [a]
foo2 = do x <- ask Z
          xs <- ask (S Z)
          return (x : xs) 

        
foo3 :: List (Cons (Z, a) (Cons (S Z, [a]) Nil)) -> [a]
foo3 = do xs <- ask (S Z)
          x <- ask Z
          return (x : xs) 
      
foo4 :: Num a => List (Cons (Z, a) Nil) -> a
foo4 = do x <- ask Z
          y <- ask Z
          return (x + y)

     
foo5 :: List (Cons (Z, a) (Cons (S Z, [a]) Nil)) -> [a]
foo5 = do xs <- ask (S Z)
          x <- ask Z
          y <- ask Z
          return (x : (y : xs))

foo6 :: List (Cons (Z, a) (Cons (S Z, [a]) Nil)) -> [a]
foo6 = do x <- ask Z
          y <- ask Z
          xs <- ask (S Z)
          return (x : (y : xs))

