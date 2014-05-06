{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, GADTs, 
             EmptyDataDecls, UndecidableInstances, RebindableSyntax, OverlappingInstances, 
             DataKinds, TypeOperators, PolyKinds, NoMonomorphismRestriction, FlexibleContexts,
             AllowAmbiguousTypes, ScopedTypeVariables, FunctionalDependencies, ConstraintKinds, 
             InstanceSigs #-}


module Control.IxMonad.State where

import Control.IxMonad
import Prelude hiding (Monad(..),reads)
import GHC.TypeLits
import Data.Proxy

data Sort = R | W 

-- Type-level list

data Nil
data Cons (k :: Symbol) (s :: Sort) (v :: *) (xs :: *)

data List n where
    Nil :: List Nil
    Cons :: Proxy (k :: Symbol) -> Proxy (s :: Sort) -> v -> List xs -> List (Cons k s v xs)

-- Type-level set union
--    implemented using lists, with a canonical ordering and duplicates removed
type family Union s t where Union s t = RemDup (BSort (Append' s t))

union :: (RemDuper (BSort (Append' s t)) (RemDup (BSort (Append' s t)))) => List s -> List t -> List (Union s t)
union s t = remDup (bsort (append s t))

-- Type-level list append
type family Append' s t where
       Append' Nil t = t
       Append' (Cons k s x xs) ys = Cons k s x (Append' xs ys)

-- Remove duplicates from a type-level list
type family RemDup t where
            RemDup Nil                        = Nil
            RemDup (Cons k s a  Nil)          = Cons k s a Nil
            RemDup (Cons k s a (Cons k s a as)) = Cons k s a (RemDup as)
            RemDup (Cons k s a (Cons j t b as)) = Cons k s a (Cons j t b (RemDup as))

class RemDuper t v where
    remDup :: List t -> List v
instance RemDuper Nil Nil where
    remDup Nil = Nil
instance RemDuper (Cons k s a Nil) (Cons k s a Nil) where
    remDup (Cons k s a Nil) = (Cons k s a Nil)

instance RemDuper as as' => RemDuper (Cons k s a (Cons k s a as)) (Cons k s a as') where
    remDup (Cons k s a (Cons _ _ _ xs)) = (Cons k s a (remDup xs))

instance RemDuper as as' => RemDuper (Cons k s a (Cons j t b as)) (Cons k s a (Cons j t b as')) where
    remDup (Cons k s a (Cons j t b xs)) = Cons k s a (Cons j t b (remDup xs))


-- Introve duplicates from a type-level list
type family IntrDup t where
            IntrDup Nil                        = Nil
            IntrDup (Cons k s a  Nil)          = Cons k s a Nil
            IntrDup (Cons k W a (Cons k W a as)) = IntrDup as
            IntrDup (Cons k R a (Cons k R a as)) = Cons k R a (IntrDup as)
            IntrDup (Cons k W a (Cons k R a as)) = Cons k R a (IntrDup as)
            IntrDup (Cons k R a (Cons k W a as)) = Cons k R a (IntrDup as)
            IntrDup (Cons k W a (Cons j R b as)) = Cons j R b (IntrDup as)
            IntrDup (Cons k R a (Cons j W b as)) = Cons k R a (IntrDup as)

class IntrDuper t v where
    intrDup :: List t -> List v
instance IntrDuper Nil Nil where
    intrDup Nil = Nil
instance IntrDuper (Cons k s a Nil) (Cons k s a Nil) where
    intrDup (Cons k s a Nil) = (Cons k s a Nil)

instance IntrDuper as as' => IntrDuper (Cons k W a (Cons k W a as)) as' where
    intrDup (Cons _ _ _ (Cons _ _ _ xs)) = intrDup xs

instance IntrDuper as as' => IntrDuper (Cons k R a (Cons k R a as)) (Cons k R a as') where
    intrDup (Cons _ _ _ (Cons k s a xs)) = Cons k s a (intrDup xs)

instance IntrDuper as as' => IntrDuper (Cons k W a (Cons k R a as)) (Cons k R a as') where
    intrDup (Cons k _ a (Cons _ _ _ xs)) = (Cons k Proxy a (intrDup xs))
instance IntrDuper as as' => IntrDuper (Cons k R a (Cons k W a as)) (Cons k R a as') where
    intrDup (Cons _ _ _ (Cons k _ a xs)) = (Cons k Proxy a (intrDup xs))

instance IntrDuper as as' => IntrDuper (Cons k W a (Cons j R b as)) (Cons j R b as') where
    intrDup (Cons _ _ _ (Cons k _ a xs)) = (Cons k Proxy a (intrDup xs))
instance IntrDuper as as' => IntrDuper (Cons k R a (Cons j W b as)) (Cons k R a as') where
    intrDup (Cons k _ a (Cons _ _ _ xs)) = (Cons k Proxy a (intrDup xs))


type family Intersect s t where Intersect s t = IntrDup (BSort (Append' s t))

intersect :: (IntrDuper (BSort (Append' s t)) (IntrDup (BSort (Append' s t)))) => List s -> List t -> List (Intersect s t)
intersect s t = intrDup (bsort (append s t))


type family BSort l where BSort l = Bubble l l

type family Bubble l l' where
    Bubble l Nil = l
    Bubble l (Cons k s a y) = Bubble (BubbleOne l) y

-- Type-level bubble sort on list
type family BubbleOne l where
            BubbleOne Nil                       = Nil
            BubbleOne (Cons k s a Nil)          = Cons k s a Nil
            BubbleOne (Cons j s a (Cons k t b xs)) = 
                       Cons (MinKey j k j k)  (MinKey j k s t) (MinKey j k a b)
                           (BubbleOne (Cons (MaxKey j k j k) (MaxKey j k s t) (MaxKey j k a b) xs))

bsort x = bubble x x

class Sortable l where
   bubble :: List (Bubble l'' l) -> List l -> List (Bubble l' l)

instance Sortable Nil where
    bubble l Nil = l

instance Sortable xs => Sortable (Cons k s a xs) where
    bubble l (Cons _ _ _ y) = bubble (bubble1 l) y

class Bubbler l where
    bubble1 :: List l -> List (BubbleOne l)

instance Bubbler Nil where
    bubble1 Nil = Nil

instance Bubbler (Cons k s a Nil) where
    bubble1 (Cons k s a Nil) = Cons k s a Nil

instance (Bubbler (Cons (MaxKey j k j k) (MaxKey j k s t) (MaxKey j k a b) 
                   xs), Chooser (CmpSymbol j k))=>
             Bubbler (Cons j s a (Cons k t b xs)) where 

 bubble1 (Cons _ _ a (Cons _ _ b xs)) = Cons Proxy Proxy (minkey (Proxy::(Proxy j)) (Proxy::(Proxy k)) a b) 
                                         (bubble1 (Cons (Proxy::(Proxy (MaxKey j k j k))) (Proxy::(Proxy (MaxKey j k s t))) (maxkey (Proxy::(Proxy j)) (Proxy::(Proxy k)) a b) xs))


minkey :: forall j k a b . 
          (Chooser (CmpSymbol j k)) => 
          Proxy j -> Proxy k -> a -> b -> MinKey j k a b
minkey _ _ x y = choose (Proxy::(Proxy (CmpSymbol j k))) x y 

maxkey :: forall j k a b . 
          (Chooser (CmpSymbol j k)) => 
          Proxy j -> Proxy k -> a -> b -> MaxKey j k a b 
maxkey _ _ a b = choose (Proxy::(Proxy (CmpSymbol j k))) b a


-- Return the minimum or maximum of two types which consistitue key-value pairs
type MinKey (a :: Symbol) (b :: Symbol) (p :: k) (q :: k) = Choose (CmpSymbol a b) p q
type MaxKey (a :: Symbol) (b :: Symbol) (p :: k) (q :: k) = Choose (CmpSymbol a b) q p

class Chooser (o :: Ordering) where
    choose :: (Proxy o) -> p -> q -> (Choose o p q)
instance Chooser LT where
    choose _ p q = p
instance Chooser EQ where
    choose _ p q = p
instance Chooser GT where
    choose _ p q = q

type family Choose (g :: Ordering) a b where
    Choose LT p q = p
    Choose EQ p q = p
    Choose GT p q = q


-- Indexed state type

data IxState s a = IxS { unIxS :: List (Reads s) -> (a, (List (Writes s))) }

type family Reads t where
    Reads Nil = Nil
    Reads (Cons k R a xs) = Cons k R a (Reads xs)
    Reads (Cons k s a xs) = Reads xs

class Readers t where 
    reads :: List t -> List (Reads t)
instance Readers Nil where
    reads Nil = Nil
instance Readers xs => Readers (Cons k R a xs) where
    reads (Cons k Proxy a xs) = Cons k Proxy a (reads xs)
instance Readers xs => Readers (Cons k W a xs) where
    reads (Cons k Proxy a xs) = reads xs

type family Writes t where
    Writes Nil = Nil
    Writes (Cons k W a xs) = Cons k W a (Writes xs)
    Writes (Cons k s a xs) = Writes xs

class Writers t where 
    writes :: List t -> List (Writes t)
instance Writers Nil where
    writes Nil = Nil
instance Writers xs => Writers (Cons k W a xs) where
    writes (Cons k Proxy a xs) = Cons k Proxy a (writes xs)
instance Writers xs => Writers (Cons k R a xs) where
    writes (Cons k Proxy a xs) = writes xs

-- 'ask' monadic primitive

get :: Proxy (k::Symbol) -> IxState (Cons k R a Nil) a
get Proxy = IxS $ \(Cons Proxy Proxy a Nil) -> (a, Nil)

put :: Proxy (k::Symbol) -> a -> IxState (Cons k W a Nil) a
put Proxy a = IxS $ \Nil -> (a, Cons Proxy Proxy a Nil)

{--

(>>=) :: IxState { x R a, y W b} a -> (a -> IxState {y R b, z W d} c)
         IxState { x R a, y U b, z W d } c

x >>= f :: [a, b] -> (c, [b, d])
  x :: [a] -> (a, [b])
  f :: a -> ([b] -> [b, d])         

--}

-- Indexed monad instance

instance IxMonad IxState where
    type Inv IxState s t = (Bubbler (Append' (Writes s) (Writes t)),
                            Bubbler (Append' (Writes s) (Reads t)), 
                            Readers t, 
                            Readers s, 
                            Split s t (Reads (Union s t)), 
                            RemDuper (BSort (Append' (Writes s) (Writes t)))
                                     (Union (Writes s) (Writes t)),
                            Readers (Reads (Union s t)), 
                            IntrDuper (BSort (Append' (Writes s) (Reads t))) (Reads t), 
                            IntrDup (BSort (Append' (Writes s) (Reads t))) ~ Reads t, 
                            Writes (Union s t) ~ Union (Writes s) (Writes t), 
                            Split (Writes s) (Writes t) (Union (Writes s) (Writes t)), 
                            Split (Reads s) (Reads t) (Reads (Union s t)))
    type Unit IxState = Nil
    type Plus IxState s t = Union s t

    return x = IxS $ \Nil -> (x, Nil)

    (>>=) :: forall s a t b . Inv IxState s t => IxState s a -> (a -> IxState t b) -> IxState (Plus IxState s t) b
    (IxS e) >>= k = 
        IxS $ \i -> 
                  let (s, t) = (split i)::(List s, List t)
                      (sR, tR) = (reads s, reads t)
                      (a, sW)  = e sR
                      (b, tW) = (unIxS $ k a) (sW `intersect` tR)
                in (b, sW `join` tW) 



-- Split operation (with type level version)

append :: List s -> List t -> List (Append' s t)
append Nil x = x
append (Cons k s x xs) ys = Cons k s x (append xs ys)

class Split s t z where
   split :: List z -> (List s, List t)
   join :: List s -> List t -> List z

instance Split Nil Nil Nil where
   split Nil = (Nil, Nil) 
   join Nil Nil = Nil

instance Split (Cons k s x xs) Nil (Cons k s x xs) where
    split t = (t, Nil)
    join t Nil = t

instance Split Nil (Cons k s x xs) (Cons k s x xs) where
   split t = (Nil, t)
   join Nil t = t

instance Split (Cons k s x Nil) (Cons k s x Nil) (Cons k s x Nil) where
   split (Cons Proxy Proxy x Nil) = (Cons Proxy Proxy x Nil, Cons Proxy Proxy x Nil)
   join _ (Cons Proxy Proxy x Nil) = Cons Proxy Proxy x Nil

instance Split xs ys zs => Split (Cons k s x xs) (Cons k s x ys) (Cons k s x zs) where
   split (Cons k s x zs) = let (xs', ys') = split zs
                           in (Cons k s x xs', Cons k s x ys')
   join (Cons _ _ _ xs) (Cons k s x ys) = Cons k s x (join xs ys)

instance (Split xs ys zs) => Split (Cons j s x xs) (Cons k t y ys) (Cons j s x (Cons k t y zs)) where
   split (Cons j s x (Cons k t y zs)) = let (xs', ys') = split zs
                                        in (Cons j s x xs', Cons k t y ys')
   join (Cons j s x xs) (Cons k t y ys) = Cons j s x (Cons k t y (join xs ys))

instance (Split xs ys zs) => Split (Cons j s x xs) (Cons k t y ys) (Cons k t y (Cons j s x zs)) where
   split (Cons j s x (Cons k t y zs)) = let (xs', ys') = split zs
                                        in (Cons k t y xs', Cons j s x ys')
   join (Cons j s x xs) (Cons k t y ys) = Cons k t y (Cons j s x (join xs ys))


foo = do x <- get (Proxy::(Proxy "x"))
         y <- get (Proxy::(Proxy "y"))
         put (Proxy::(Proxy "y")) (x : y)
         z <- get (Proxy::(Proxy "y"))
         return (x : z)