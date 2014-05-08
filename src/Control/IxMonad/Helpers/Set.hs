{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators, TypeFamilies, 
             MultiParamTypeClasses, FlexibleInstances, PolyKinds, FlexibleContexts,
             ScopedTypeVariables, UndecidableInstances, ConstraintKinds, EmptyDataDecls, 
             IncoherentInstances #-}

module Control.IxMonad.Helpers.Set (Set(..), Proxy(..), Union, Unionable, (:->), 
                                       Symbol, union) where

import GHC.TypeLits
import Data.Proxy
import Data.Monoid

data Pair (k :: Symbol) (v :: *) 

type a :-> b = Pair a b

data Set (n :: [*]) where
    Empty :: Set '[]
    Ext :: Proxy (k :: Symbol) -> v -> Set s -> Set ((k :-> v) ': s)

instance Show (Set '[]) where
    show Empty = "{}"

instance (Show (Proxy k), Show v, Show' (Set s)) => 
           Show (Set ((Pair k v) ': s)) where
    show (Ext k v s) = "{(" ++ show k ++ ", " ++ show v ++ ")" ++ (show' s) ++ "}" 

class Show' t where
    show' :: t -> String

instance Show' (Set '[]) where
    show' Empty = ""

instance (Show' (Set s), Show (Proxy k), Show v) => Show' (Set ((k :-> v) ': s)) where
    show' (Ext k v s) = ", (" ++ show k ++ ", " ++ show v ++ ")" ++ (show' s) 

-- Type-level set disjoint-union 

type family DisjUnion (s :: [*]) (t :: [*]) :: [*]
type instance DisjUnion '[] t = t
type instance DisjUnion ((k :-> v) ': xs) ys = (Pair k v) ': (DisjUnion xs ys)

-- Value-level set disjoint union

disjunion :: Set s -> Set t -> Set (DisjUnion s t)
disjunion Empty x = x
disjunion (Ext k v xs) ys = Ext k v (disjunion xs ys)


-- Type and value-level removing of duplicates from the set representation

type family RemDups t where
    RemDups '[]                 = '[]
    RemDups '[k :-> v]          = '[k :-> v]
    RemDups ((k :-> u) ': (k :-> u) ': s) = RemDups ((k :-> u) ':s)
    RemDups ((k :-> v) ': (j :-> u) ': s) = (k :-> v) ': (RemDups ((j :-> u) ': s))

type RemoveDuplicates t = RemDuper t (RemDups t)

class RemDuper t v where
    remDup :: Set t -> Set v

instance RemDuper '[] '[] where
    remDup Empty = Empty

instance RemDuper '[k :-> v] '[k :-> v] where
    remDup (Ext k v Empty) = Ext k v Empty

instance (Monoid u, RemDuper ((k :-> u) ': s) s') => RemDuper ((k :-> u) ': (k :-> u) ': s) s' where
    remDup (Ext _ u (Ext k v s)) = remDup (Ext k (u `mappend` v) s)

instance RemDuper ((j :-> u) ': s) s' => RemDuper ((k :-> v) ': (j :-> u) ': s) ((k :-> v) ': s') where
    remDup (Ext k v (Ext j u s)) = Ext k v (remDup (Ext j u s))

-- Type- and value-level set union

type Union s t = RemDups (BSort (DisjUnion s t))

type Unionable s t = (Sortable (DisjUnion s t), RemoveDuplicates (BSort (DisjUnion s t)))

union :: (Sortable (DisjUnion s t), RemoveDuplicates (BSort (DisjUnion s t))) => 
         Set s -> Set t -> Set (Union s t)
union s t = remDup (bsort (disjunion s t))

-- Bubble sort for normalising the representation

type BSort l = Bubble l l

type family Bubble l l' where
    Bubble l '[] = l
    Bubble l (a ': y) = BubbleOne (Bubble l y)

-- Type-level bubble sort on list
type family BubbleOne l where
            BubbleOne '[]                 = '[]
            BubbleOne '[k :-> v]          = '[k :-> v]
            BubbleOne ((j :-> u) ': ((k :-> v) ': s)) = 
                       ((MinKey j k j k) :-> (MinKey j k u v)) ': 
                           (BubbleOne (((MaxKey j k j k) :-> (MaxKey j k u v)) ': s))

type Sortable s = Bubbler s s

class Bubbler s s' where
    bubble :: Set s -> Set s' -> Set (Bubble s s')

instance Bubbler s '[] where
    bubble s Empty = s

instance (Bubbler s t, Bubbler1 (Bubble s t)) => Bubbler s ((k :-> v) ': t) where
    bubble s (Ext _ _ t) = bubble1 (bubble s t)

bsort :: (Bubbler s s) => Set s -> Set (BSort s)
bsort x = bubble x x

class Bubbler1 s where
    bubble1 :: Set s -> Set (BubbleOne s)

instance Bubbler1 '[] where
    bubble1 Empty = Empty

instance Bubbler1 '[(k :-> v)] where
    bubble1 (Ext k v Empty) = Ext k v Empty

instance (Bubbler1 (((MaxKey j k j k) :-> (MaxKey j k u v)) ': s), Chooser (CmpSymbol j k))=>
             Bubbler1 ((j :-> u) ': (k :-> v) ': s) where 

    bubble1 (Ext _ u (Ext _ v s)) = Ext Proxy (minkey (Proxy::(Proxy j)) (Proxy::(Proxy k)) u v)
                                         (bubble1 (Ext (Proxy::(Proxy (MaxKey j k j k))) (maxkey (Proxy::(Proxy j)) (Proxy::(Proxy k)) u v) s))


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
