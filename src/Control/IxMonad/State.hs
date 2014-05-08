{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, GADTs, 
             EmptyDataDecls, UndecidableInstances, RebindableSyntax, OverlappingInstances, 
             DataKinds, TypeOperators, PolyKinds, NoMonomorphismRestriction, FlexibleContexts,
             AllowAmbiguousTypes, ScopedTypeVariables, FunctionalDependencies, ConstraintKinds, 
             InstanceSigs, IncoherentInstances #-}

module Control.IxMonad.State where

import Control.IxMonad
import Prelude hiding (Monad(..),reads)
import GHC.TypeLits
import Data.Proxy
import Debug.Trace

-- Distinguish reads, writes, and read-writes
data Sort = R | W | RW

-- Type-level set representation as a list of triples
data Nil
data Cons (k :: Symbol) (s :: Sort) (v :: *) (xs :: *)

data List n where
    Nil :: List Nil
    Cons :: Proxy (k :: Symbol) -> Proxy (s :: Sort) -> v -> List xs -> List (Cons k s v xs)

-- Printing 
instance Show (List Nil) where
    show Nil = "Nil"
instance (ShowMore (Proxy k), 
          ShowMore (Proxy s), Show v, Show (List xs)) => Show (List (Cons k s v xs)) where
    show (Cons k s v xs) = "Cons " ++ (showM k) ++ " " ++ (showM s) ++ " " ++ (show v) ++ " " ++ show xs

-- Mostly used for debugging
class ShowMore t where
    showM :: t -> String
instance ShowMore (Proxy R) where
    showM _ = "R"
instance ShowMore (Proxy W) where
    showM _ = "W"
instance ShowMore (Proxy RW) where
    showM _ = "RW"
instance ShowMore (Proxy "x") where
    showM _ = "x"
instance ShowMore (Proxy "y") where
    showM _ = "y"

-- Type-level list append
type family Append s t where
       Append Nil t = t
       Append (Cons k s x xs) ys = Cons k s x (Append xs ys)

append :: List s -> List t -> List (Append s t)
append Nil x = x
append (Cons k s x xs) ys = Cons k s x (append xs ys)

-- Type-level set union
--    implemented using lists, with a canonical ordering and duplicates removed
--    and turning of 'R' and 'W' on the same key into 'RW'

type Unionable s t = RemDuper (BSort (Append s t)) (Union s t)

type family Union s t where Union s t = RemDup (BSort (Append s t))

union :: (Sortable (Append s t), RemDuper (BSort (Append s t)) (RemDup (BSort (Append s t)))) => List s -> List t -> List (Union s t)
union s t = remDup (bsort (append s t))

-- Remove duplicates from a type-level list and turn different sorts into 'RW'
type family RemDup t where
            RemDup Nil                        = Nil
            RemDup (Cons k s a  Nil)          = Cons k s a Nil
            RemDup (Cons k s a (Cons k s b as)) = RemDup (Cons k s b as)
            RemDup (Cons k s a (Cons k t a as)) = RemDup (Cons k RW a as)
            RemDup (Cons k s a (Cons j t b as)) = Cons k s a (RemDup (Cons j t b as))

class RemDuper t v where
    remDup :: List t -> List v
instance RemDuper Nil Nil where
    remDup Nil = Nil
instance RemDuper (Cons k s a Nil) (Cons k s a Nil) where
    remDup (Cons k s a Nil) = (Cons k s a Nil)
instance RemDuper (Cons k s b as) as' => RemDuper (Cons k s a (Cons k s b as)) as' where
    remDup (Cons _ _ _ (Cons k s a xs)) = remDup (Cons k s a xs)
instance RemDuper (Cons k RW b as) as' => RemDuper (Cons k s a (Cons k t b as)) as' where
    remDup (Cons _ _ _ (Cons k _ a xs)) = remDup (Cons k (Proxy::(Proxy RW)) a xs)
instance RemDuper (Cons j t b as) as' => RemDuper (Cons k s a (Cons j t b as)) (Cons k s a as') where
    remDup (Cons k s a (Cons j t b xs)) = Cons k s a (remDup (Cons j t b xs))

-- Replaces reads with writes
type family IntrDup t where
            IntrDup Nil                        = Nil
            IntrDup (Cons k W a  Nil)          = Nil
            IntrDup (Cons k s a  Nil)          = Cons k s a Nil
            IntrDup (Cons k s a (Cons k s b as)) = IntrDup (Cons k R b as)

            IntrDup (Cons k W a (Cons k R b as)) = IntrDup (Cons k R a as)
            IntrDup (Cons k R a (Cons k W b as)) = IntrDup (Cons k R b as)

            IntrDup (Cons k RW a (Cons k R b as)) = IntrDup (Cons k R a as)
            IntrDup (Cons k R a (Cons k RW b as)) = IntrDup (Cons k R b as)


            IntrDup (Cons k W a (Cons j R b as)) = IntrDup (Cons j R b as)
            IntrDup (Cons k R a (Cons j W b as)) = IntrDup (Cons k R a as)
            IntrDup (Cons k W a (Cons j W b as)) = IntrDup as
            IntrDup (Cons k R a (Cons j R b as)) = Cons k R a (IntrDup (Cons j R b as))

class IntrDuper t v where
    intrDup :: List t -> List v

instance IntrDuper Nil Nil where
    intrDup Nil = Nil

instance IntrDuper (Cons k W a Nil) Nil where
    intrDup (Cons k s a Nil) = Nil

instance IntrDuper (Cons k s a Nil) (Cons k s a Nil) where
    intrDup (Cons k s a Nil) = (Cons k s a Nil)

instance IntrDuper (Cons k R b as) as' => IntrDuper (Cons k R a (Cons k R b as)) as' where
    intrDup (Cons _ _ _ (Cons k s a xs)) = intrDup (Cons k (Proxy::(Proxy R)) a xs)

instance IntrDuper (Cons k R b as) as' => IntrDuper (Cons k W a (Cons k W b as)) as' where
    intrDup (Cons _ _ _ (Cons k s a xs)) = intrDup (Cons k (Proxy::(Proxy R)) a xs)

instance IntrDuper (Cons k R b as) as' => IntrDuper (Cons k RW a (Cons k RW b as)) as' where
    intrDup (Cons _ _ _ (Cons k s a xs)) = intrDup (Cons k (Proxy::(Proxy R)) a xs)

instance IntrDuper (Cons k R a as) as' => IntrDuper (Cons k W a (Cons k R b as)) as' where
    intrDup (Cons k _ a (Cons _ _ _ xs)) = intrDup (Cons k (Proxy::(Proxy R)) a xs)

instance IntrDuper (Cons k R b as) as' => IntrDuper (Cons k R a (Cons k W b as)) as' where
    intrDup (Cons _ _ _ (Cons k _ a xs)) = intrDup (Cons k (Proxy::(Proxy R)) a xs)

instance IntrDuper (Cons k R a as) as' => IntrDuper (Cons k RW a (Cons k R b as)) as' where
    intrDup (Cons k _ a (Cons _ _ _ xs)) = intrDup (Cons k (Proxy::(Proxy R)) a xs)

instance IntrDuper (Cons k R b as) as' => IntrDuper (Cons k R a (Cons k RW b as)) as' where
    intrDup (Cons _ _ _ (Cons k _ a xs)) = intrDup (Cons k (Proxy::(Proxy R)) a xs)

instance IntrDuper (Cons j R b as) as' => IntrDuper (Cons k W a (Cons j R b as)) as' where
    intrDup (Cons _ _ _ (Cons k _ a xs)) = intrDup (Cons k (Proxy::(Proxy R)) a xs)

instance IntrDuper (Cons k R a as) as' => IntrDuper (Cons k R a (Cons j W b as)) as' where
    intrDup (Cons k _ a (Cons _ _ _ xs)) = intrDup (Cons k (Proxy::(Proxy R)) a xs)

instance IntrDuper as as' => IntrDuper (Cons k W a (Cons j W b as)) as' where
    intrDup (Cons _ _ _ (Cons _ _ _ xs)) = intrDup xs

instance IntrDuper (Cons j R b as) as' => 
           IntrDuper (Cons k R a (Cons j R b as)) (Cons k R a as') where
    intrDup (Cons k s a (Cons j t b xs)) = Cons k s a (intrDup (Cons j t b xs))


type family Intersect s t where Intersect s t = IntrDup (BSort (Append s t))

type Intersectable s t = IntrDuper (BSort (Append s t)) t

intersect :: (Sortable (Append s t), IntrDuper (BSort (Append s t)) (IntrDup (BSort (Append s t)))) => List s -> List t -> List (Intersect s t)
intersect s t = intrDup (bsort (append s t))


type family BSort l where BSort l = Bubble l l

type family Bubble l l' where
    Bubble l Nil = l
    Bubble l (Cons k s a y) = BubbleOne (Bubble l y)

-- Type-level bubble sort on list
type family BubbleOne l where
            BubbleOne Nil                       = Nil
            BubbleOne (Cons k s a Nil)          = Cons k s a Nil
            BubbleOne (Cons j s a (Cons k t b xs)) = 
                       Cons (MinKey j k j k)  (MinKey j k s t) (MinKey j k a b)
                           (BubbleOne (Cons (MaxKey j k j k) (MaxKey j k s t) (MaxKey j k a b) xs))

type Sortable l = Bubbler l l

class Bubbler l l' where
    bubble :: List l -> List l' -> List (Bubble l l')

instance Bubbler l Nil where
    bubble l Nil = l

instance (Bubbler l y, Bubbler1 (Bubble l y)) => Bubbler l (Cons k s a y) where
    bubble l (Cons k s a y) = bubble1 (bubble l y)

bsort :: (Bubbler l l) => List l -> List (BSort l)
bsort x = bubble x x

class Bubbler1 l where
    bubble1 :: List l -> List (BubbleOne l)

instance Bubbler1 Nil where
    bubble1 Nil = Nil

instance Bubbler1 (Cons k s a Nil) where
    bubble1 (Cons k s a Nil) = Cons k s a Nil

instance (Bubbler1 (Cons (MaxKey j k j k) (MaxKey j k s t) (MaxKey j k a b) 
                   xs), Chooser (CmpSymbol j k))=>
             Bubbler1 (Cons j s a (Cons k t b xs)) where 

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
data IxState s a = IxS { runIxState :: List (Reads s) -> (a, (List (Writes s))) }

type family Reads t where
    Reads Nil = Nil
    Reads (Cons k R a xs) = Cons k R a (Reads xs)
    Reads (Cons k RW a xs) = Cons k R a (Reads xs)
    Reads (Cons k s a xs) = Reads xs

class Readers t where 
    reads :: List t -> List (Reads t)
instance Readers Nil where
    reads Nil = Nil
instance Readers xs => Readers (Cons k R a xs) where
    reads (Cons k Proxy a xs) = Cons k Proxy a (reads xs)
instance Readers xs => Readers (Cons k RW a xs) where
    reads (Cons k Proxy a xs) = Cons k Proxy a (reads xs)
instance Readers xs => Readers (Cons k W a xs) where
    reads (Cons k Proxy a xs) = reads xs

type family Writes t where
    Writes Nil = Nil
    Writes (Cons k W a xs) = Cons k W a (Writes xs)
    Writes (Cons k RW a xs) = Cons k W a (Writes xs)
    Writes (Cons k s a xs) = Writes xs

-- 'ask' monadic primitive

get :: Proxy (k::Symbol) -> IxState (Cons k R a Nil) a
get Proxy = IxS $ \(Cons Proxy Proxy a Nil) -> (a, Nil)

put :: Proxy (k::Symbol) -> a -> IxState (Cons k W a Nil) ()
put Proxy a = IxS $ \Nil -> ((), Cons Proxy Proxy a Nil)


-- Indexed monad instance
instance IxMonad IxState where
    type Inv IxState s t = (Split (Reads s) (Reads t) (Reads (Reads (Union s t))), 
                            Sortable (Append (Writes s) (Writes t)), 
                            Sortable (Append (Writes s) (Reads t)), 
                            Unionable (Writes s) (Writes t), 
                            Readers (Reads (Union s t)), 
                            Intersectable (Writes s) (Reads t), 
                            Intersect (Writes s) (Reads t) ~ Reads t, 
                            Writes (Union s t) ~ Union (Writes s) (Writes t))
    type Unit IxState = Nil
    type Plus IxState s t = Union s t

    return x = IxS $ \Nil -> (x, Nil)

    (>>=) :: forall s a t b . Inv IxState s t => 
             IxState s a -> (a -> IxState t b) -> IxState (Plus IxState s t) b
    (IxS e) >>= k = 
        IxS $ \i -> 
                  let (sR, tR) = (split (reads i)) ::(List (Reads s), List (Reads t))
                      (a, sW)  = e sR
                      (b, tW) = (runIxState $ k a) (sW `intersect` tR)
                in (b, sW `union` tW) 

-- Split operation (with type level version)

class Split s t z where
   split :: List z -> (List s, List t)

instance Split Nil Nil Nil where
   split Nil = (Nil, Nil) 

instance Split (Cons k s x xs) Nil (Cons k s x xs) where
    split t = (t, Nil)

instance Split Nil (Cons k s x xs) (Cons k s x xs) where
   split t = (Nil, t)

instance Split (Cons k s x Nil) (Cons k t x Nil) (Cons k r x Nil) where
   split (Cons Proxy Proxy x Nil) = (Cons Proxy Proxy x Nil, Cons Proxy Proxy x Nil)

instance Split xs ys zs => Split (Cons k s x xs) (Cons k s x ys) (Cons k s x zs) where
   split (Cons k s x zs) = let (xs', ys') = split zs
                           in (Cons k s x xs', Cons k s x ys')

instance (Split xs ys zs) => Split (Cons j s x xs) (Cons k t y ys) (Cons j s' x (Cons k t' y zs)) where
   split (Cons j s x (Cons k t y zs)) = let (xs', ys') = split zs
                                        in (Cons j Proxy x xs', Cons k Proxy y ys')

instance (Split xs ys zs) => Split (Cons j s x xs) (Cons k t y ys) (Cons k t' y (Cons j s' x zs)) where
   split (Cons j s x (Cons k t y zs)) = let (xs', ys') = split zs
                                        in (Cons k Proxy y xs', Cons j Proxy x ys')


foo :: IxState (Cons "x" R Int (Cons "y" RW [Int] Nil)) [Int]
foo = do x <- get (Proxy::(Proxy "x"))
         y <- get (Proxy::(Proxy "y"))
         put (Proxy::(Proxy "y")) (x:y)
         z <- get (Proxy::(Proxy "y"))
         return (x:z)

foo_run = runIxState foo (Cons x r 1 (Cons y r [2,3] Nil))

foo2 :: IxState (Cons "x" RW Int Nil) Int
foo2 = do x <- get (Proxy::(Proxy "x"))
          put (Proxy::(Proxy "x")) (x+1)
          return x

foo2_run = (runIxState foo2) (Cons x r 10 Nil)

x = Proxy::(Proxy "x")
y = Proxy::(Proxy "y")
r = Proxy::(Proxy R)
w = Proxy::(Proxy W)