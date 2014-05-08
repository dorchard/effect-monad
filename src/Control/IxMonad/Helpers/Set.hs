{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators, TypeFamilies, 
             MultiParamTypeClasses, FlexibleInstances, PolyKinds, FlexibleContexts,
             ScopedTypeVariables, UndecidableInstances, ConstraintKinds, EmptyDataDecls, 
             IncoherentInstances #-}

module Control.IxMonad.Helpers.Set (Set(..), Proxy(..), Union, Unionable, (:->), 
                                       Symbol, union, Var(..)) where

import GHC.TypeLits
import Data.Proxy
import Data.Monoid

data (k :: Symbol) :-> (v :: *) = (Var k) :->  v

data Var (k :: Symbol) = Var

data Set (n :: [*]) where
    Empty :: Set '[]
    Ext :: e -> Set s -> Set (e ': s)

-- ****** Type level *******

-- Set disjoint union (essentially list append)

type family DisjUnion (s :: [*]) (t :: [*]) :: [*]
type instance DisjUnion '[] t = t
type instance DisjUnion (x ': xs) ys = x ': (DisjUnion xs ys)

type family RemDups t where
    RemDups '[]           = '[]
    RemDups '[e]          = '[e]
    RemDups (e ': e ': s) = RemDups (e ': s)
    RemDups (e ': f ': s) = e ': RemDups (f ': s)

type Union s t = RemDups (Sort (DisjUnion s t))

type Unionable s t = (Sortable (DisjUnion s t), RemDuper (Sort (DisjUnion s t)) (RemDups (Sort (DisjUnion s t))))

-- Sorting for normalising the representation

type Sortable s = Bubbler s s

type Sort l = Bubble l l

type family Bubble l l' where
    Bubble l '[] = l
    Bubble l (x ': xs) = Pass (Bubble l xs)

-- Type-level bubble sort on list
type family Pass (l :: [*]) :: [*]
type instance Pass '[]           = '[]
type instance Pass '[e]          = '[e]
type instance Pass ((j :-> u) ': ((k :-> v) ': s)) = 
                       ((MinBy j k j k) :-> (MinBy j k u v)) ': 
                           Pass (((MaxBy j k j k) :-> (MaxBy j k u v)) ': s)


-- Return the minimum or maximum of two types which consistitue key-value pairs
type MinBy (a :: Symbol) (b :: Symbol) (p :: k) (q :: k) = ((Choose (CmpSymbol a b) p q) :: k)
type MaxBy (a :: Symbol) (b :: Symbol) (p :: k) (q :: k) = ((Choose (CmpSymbol a b) q p) :: k)

type family Choose (g :: Ordering) a b where
    Choose LT p q = p
    Choose EQ p q = p
    Choose GT p q = q

-- ******* Value level *********

-- Showing for debugging

instance Show (Set '[]) where
    show Empty = "{}"

instance (Show (Var k), Show v, Show' (Set s)) => 
           Show (Set ((k :-> v) ': s)) where
    show (Ext (k :-> v) s) = "{(" ++ show k ++ ", " ++ show v ++ ")" ++ (show' s) ++ "}" 

class Show' t where
    show' :: t -> String

instance Show' (Set '[]) where
    show' Empty = ""

instance (Show' (Set s), Show (Var k), Show v) => Show' (Set ((k :-> v) ': s)) where
    show' (Ext (k :-> v) s) = ", (" ++ show k ++ ", " ++ show v ++ ")" ++ (show' s) 



-- Set disjoint union

disjunion :: Set s -> Set t -> Set (DisjUnion s t)
disjunion Empty x = x
disjunion (Ext e xs) ys = Ext e (disjunion xs ys)


-- Removing of duplicates from the set representation

class RemDuper t v where
    remDup :: Set t -> Set v

instance RemDuper '[] '[] where
    remDup Empty = Empty

instance RemDuper '[k :-> v] '[k :-> v] where
    remDup (Ext (k :-> v) Empty) = Ext (k :-> v) Empty

instance (Monoid u, RemDuper ((k :-> u) ': s) s') => RemDuper ((k :-> u) ': (k :-> u) ': s) s' where
    remDup (Ext (_ :-> u) (Ext (k :-> v) s)) = remDup (Ext (k :-> (u `mappend` v)) s)

instance RemDuper ((j :-> u) ': s) s' => RemDuper ((k :-> v) ': (j :-> u) ': s) ((k :-> v) ': s') where
    remDup (Ext (k :-> v) (Ext (j :-> u) s)) = Ext (k :-> v) (remDup (Ext (j :-> u) s))

-- Set union

union :: (Unionable s t) => Set s -> Set t -> Set (Union s t)
union s t = remDup (bsort (disjunion s t))

class Bubbler s s' where
    bubble :: Set s -> Set s' -> Set (Bubble s s')

instance Bubbler s '[] where
    bubble s Empty = s

instance (Bubbler s t, Bubbler1 (Bubble s t)) => Bubbler s (e ': t) where
    bubble s (Ext _ t) = bubble1 (bubble s t)

bsort :: (Bubbler s s) => Set s -> Set (Sort s)
bsort x = bubble x x

class Bubbler1 s where
    bubble1 :: Set s -> Set (Pass s)

instance Bubbler1 '[] where
    bubble1 Empty = Empty

instance Bubbler1 e where
    bubble1 (Ext e Empty) = Ext e Empty

instance (Bubbler1 (((MaxBy j k j k) :-> (MaxBy j k u v)) ': s), Chooser (CmpSymbol j k))=>
             Bubbler1 ((j :-> u) ': (k :-> v) ': s) where 

    bubble1 (Ext (_ :-> u) (Ext (_ :-> v) s)) = Ext (Var :-> (minkey (Var::(Var j)) (Var::(Var k)) u v))
                                         (bubble1 (Ext ((Var::(Var (MaxBy j k j k))) :-> (maxkey (Var::(Var j)) (Var::(Var k)) u v)) s))


minkey :: forall j k a b . 
          (Chooser (CmpSymbol j k)) => 
          Var j -> Var k -> a -> b -> MinBy j k a b
minkey _ _ x y = choose (Proxy::(Proxy (CmpSymbol j k))) x y 

maxkey :: forall j k a b . 
          (Chooser (CmpSymbol j k)) => 
          Var j -> Var k -> a -> b -> MaxBy j k a b 
maxkey _ _ a b = choose (Proxy::(Proxy (CmpSymbol j k))) b a


class Chooser (o :: Ordering) where
    choose :: (Proxy o) -> p -> q -> (Choose o p q)
instance Chooser LT where
    choose _ p q = p
instance Chooser EQ where
    choose _ p q = p
instance Chooser GT where
    choose _ p q = q

