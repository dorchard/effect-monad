{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, GADTs, 
             EmptyDataDecls, UndecidableInstances, RebindableSyntax, OverlappingInstances, 
             DataKinds, TypeOperators, PolyKinds, NoMonomorphismRestriction, FlexibleContexts,
             AllowAmbiguousTypes, ScopedTypeVariables, FunctionalDependencies #-}

module Control.IxMonad.State where

import Control.IxMonad
import Prelude hiding (Monad(..),reads)
import GHC.TypeLits
import Data.Proxy

data Sort = R | W | U

-- Type-level list

data Nil
data Cons (k :: Symbol) (s :: Sort) (v :: *) (xs :: *)

data List n where
    Nil :: List Nil
    Cons :: Proxy (k :: Symbol) -> Proxy (s :: Sort) -> v -> List xs -> List (Cons k s v xs)

-- Type-level set union
--    implemented using lists, with a canonical ordering and duplicates removed
type family Union s t where Union s t = RemDup (Bubble (Append' s t))

union :: (RemDuper (Bubble (Append' s t)) (RemDup (Bubble (Append' s t))), 
             Bubbler (Append' s t)) => List s -> List t -> List (Union s t)
union s t = remDup (bubble (append s t))

-- Type-level list append
type family Append' s t where
       Append' Nil t = t
       Append' (Cons k s x xs) ys = Cons k s x (Append' xs ys)

-- Remove duplicates from a type-level list
type family RemDup t where
            RemDup Nil                        = Nil
            RemDup (Cons k s a  Nil)          = Cons k s a Nil
            RemDup (Cons k s a (Cons k s a as)) = Cons k s a (RemDup as)
            RemDup (Cons k s a (Cons k t a as)) = Cons k U a (RemDup as)
            RemDup (Cons k s a (Cons j t b as)) = Cons k s a (Cons j t b (RemDup as))

class RemDuper t v where
    remDup :: List t -> List v
instance RemDuper Nil Nil where
    remDup Nil = Nil
instance RemDuper (Cons k s a Nil) (Cons k s a Nil) where
    remDup (Cons k s a Nil) = (Cons k s a Nil)
instance RemDuper as as' => RemDuper (Cons k s a (Cons k s a as)) (Cons k s a as') where
    remDup (Cons k s a (Cons _ _ _ xs)) = (Cons k s a (remDup xs))
instance RemDuper as as' => RemDuper (Cons k R a (Cons k W a as)) (Cons k U a as') where
    remDup (Cons _ _ _ (Cons k t a xs)) = Cons k Proxy a (remDup xs)
instance RemDuper as as' => RemDuper (Cons k R a (Cons k U a as)) (Cons k U a as') where 
    remDup (Cons _ _ _ (Cons k t a xs)) = Cons k Proxy a (remDup xs)
instance RemDuper as as' => RemDuper (Cons k W a (Cons k R a as)) (Cons k U a as') where 
    remDup (Cons _ _ _ (Cons k t a xs)) = Cons k Proxy a (remDup xs)
instance RemDuper as as' => RemDuper (Cons k W a (Cons k U a as)) (Cons k U a as') where
    remDup (Cons _ _ _ (Cons k t a xs)) = Cons k Proxy a (remDup xs)
instance RemDuper as as' => RemDuper (Cons k s a (Cons j t b as)) (Cons k s a (Cons j t b as')) where
    remDup (Cons k s a (Cons j t b xs)) = Cons k s a (Cons j t b (remDup xs))


-- Type-level bubble sort on list
type family Bubble l where
            Bubble Nil                       = Nil
            Bubble (Cons k s a Nil)          = Cons k s a Nil
            Bubble (Cons j s a (Cons k t b xs)) = 
                       Cons (MinKey j k j k)  (MinKey j k s t) (MinKey j k a b)
                           (Bubble (Cons (MaxKey j k j k) (MaxKey j k s t) (MaxKey j k a b) xs))

class Bubbler l where
    bubble :: List l -> List (Bubble l)

instance Bubbler Nil where
    bubble Nil = Nil

instance Bubbler (Cons k s a Nil) where
    bubble (Cons k s a Nil) = Cons k s a Nil

instance (Bubbler (Cons (MaxKey j k j k) (MaxKey j k s t) (MaxKey j k a b) 
                   xs), Chooser (CmpSymbol j k))=>
             Bubbler (Cons j s a (Cons k t b xs)) where 

 bubble (Cons _ _ a (Cons _ _ b xs)) = Cons Proxy Proxy (minkey (Proxy::(Proxy j)) (Proxy::(Proxy k)) a b) 
                                         (bubble (Cons (Proxy::(Proxy (MaxKey j k j k))) (Proxy::(Proxy (MaxKey j k s t))) (maxkey (Proxy::(Proxy j)) (Proxy::(Proxy k)) a b) xs))


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

data IxState s a = IxS { unIxS :: (List (Union (Reads s) (Updates s))) -> (a, (List (Union (Writes s) (Updates s)))) }

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
instance Readers xs => Readers (Cons k U a xs) where
    reads (Cons k Proxy a xs) = reads xs
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
instance Writers xs => Writers (Cons k U a xs) where
    writes (Cons k Proxy a xs) = writes xs
instance Writers xs => Writers (Cons k R a xs) where
    writes (Cons k Proxy a xs) = writes xs

type family Updates t where
    Updates Nil = Nil
    Updates (Cons k U a xs) = Cons k U a (Updates xs)
    Updates (Cons k s a xs) = Updates xs

class Updaters t where 
    updates :: List t -> List (Updates t)
instance Updaters Nil where
    updates Nil = Nil
instance Updaters xs => Updaters (Cons k U a xs) where
    updates (Cons k Proxy a xs) = Cons k Proxy a (updates xs)
instance Updaters xs => Updaters (Cons k R a xs) where
    updates (Cons k Proxy a xs) = updates xs
instance Updaters xs => Updaters (Cons k W a xs) where
    updates (Cons k Proxy a xs) = updates xs 

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
    type Inv IxState s t = (
                            (Union (Writes (Union s t)) (Updates (Union s t)))
                      ~ Bubble
                          (Union (Union 
                                   (Union (Writes (Union (Writes s) (Updates s)))
                                          (Updates (Union (Writes s) (Updates s))))
                                       (Writes (Union (Writes t) (Updates t))))
                                 (Updates (Union (Writes t) (Updates t)))))

--                         (Append (Reads t) (Updates t)
--                      ~ Append s1 (Updates (Append (Writes s) (Updates s))))

 -- , Split (Reads s) (Reads t) (Reads (Union s t)))

    type Unit IxState = Nil
    type Plus IxState s t = Union s t

    return x = IxS $ \Nil -> (x, Nil)

    (IxS e) >>= k = 
        IxS $ \i -> 
                  let (s, t) = split i
                      (sR, tR) = (reads s, reads t)
                      (sU, tU) = (updates s, updates t)
                      (a, o1)  = e (sR `union` sU)
                      w1 = writes o1
                      u1 = updates o1
                      (b, o2) = (unIxS $ k a) (tR `union` tU `union` u1)
                      w2 = writes o2
                      u2 = updates o2
                      w = (w1 `union` u1 `union` w2 `union` u2)
                in (b, w)

foob' k e = \i -> 
                  let (sR, tR) = split $ reads i
                      (sU, tU) = split $ updates i
                      (a, o1)  = e $ sR `append` sU
                      w1 = writes o1
                      u1 = updates o1
                      (b, o2) = (k a) (tR `append` u1)
                      w2 = writes o2
                      u2 = writes o2
                      w = bubble (w1 `union` w2 `union` u2)
                in (sR, tR, sU, tU, w)

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