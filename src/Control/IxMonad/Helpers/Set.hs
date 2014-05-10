{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators, TypeFamilies, 
             MultiParamTypeClasses, FlexibleInstances, PolyKinds, FlexibleContexts,
             UndecidableInstances, IncoherentInstances, ConstraintKinds #-}

module Control.IxMonad.Helpers.Set (Set(..), Union, Unionable, union, bsort, append, Sort, Sortable, 
                                    RemDuper(..), OrdH(..), Min, Max, Append(..), Split(..)) where

{- Core Set definition, in terms of lists -}

data Set (n :: [*]) where
    Empty :: Set '[]
    Ext :: e -> Set s -> Set (e ': s)

{-- Union --}

type Union s t = RemDups (Sort (Append s t))

union :: (Unionable s t) => Set s -> Set t -> Set (Union s t)
union s t = remDup (bsort (append s t))

type Unionable s t = (Sortable (Append s t), RemDuper (Sort (Append s t)) (RemDups (Sort (Append s t))))
type Sortable s = Bubbler s s

{- List append (essentially set disjoint union) -}

type family Append (s :: [*]) (t :: [*]) :: [*]
type instance Append '[] t = t
type instance Append (x ': xs) ys = x ': (Append xs ys)

append :: Set s -> Set t -> Set (Append s t)
append Empty x = x
append (Ext e xs) ys = Ext e (append xs ys)

{- Remove duplicates-}

type family RemDups t where
    RemDups '[]           = '[]
    RemDups '[e]          = '[e]
    RemDups (e ': e ': s) = RemDups (e ': s)
    RemDups (e ': f ': s) = e ': RemDups (f ': s)

class RemDuper t v where
    remDup :: Set t -> Set v

instance RemDuper '[] '[] where
    remDup Empty = Empty

instance RemDuper '[e] '[e] where
    remDup (Ext x Empty) = Ext x Empty

instance RemDuper (f ': s) s' => RemDuper (e ': f ': s) (e ': s') where
    remDup (Ext e (Ext f s)) = Ext e (remDup (Ext f s))

{- Sorting for normalising the representation -}

-- Top-level

type Sort l = Bubble l l

bsort :: (Bubbler s s) => Set s -> Set (Sort s)
bsort x = bubble x x

-- Iterate

type family Bubble l l' where
    Bubble l '[] = l
    Bubble l (x ': xs) = Pass (Bubble l xs)

class Bubbler s s' where
    bubble :: Set s -> Set s' -> Set (Bubble s s')

instance Bubbler s '[] where
    bubble s Empty = s

instance (Bubbler s t, Passer (Bubble s t)) => Bubbler s (e ': t) where
    bubble s (Ext _ t) = pass (bubble s t)

{- Single-pass of the bubble sort -}

type family Pass (l :: [*]) :: [*]
type instance Pass '[]           = '[]
type instance Pass '[e]          = '[e]
type instance Pass (e ': f ': s) = Min e f ': (Pass ((Max e f) ': s))

class Passer s where
    pass :: Set s -> Set (Pass s)

instance Passer '[] where
    pass Empty = Empty

instance Passer '[e] where
    pass (Ext e Empty) = Ext e Empty

instance (Passer ((Max e f) ': s), OrdH e f)=> Passer (e ': f ': s) where 
    pass (Ext e (Ext f s)) = Ext (minH e f) (pass (Ext (maxH e f) s))

{- Ordering for the sort -}

type family Min (a :: k) (b :: k) :: k
type family Max (a :: k) (b :: k) :: k

class OrdH e f where
    minH :: e -> f -> Min e f
    maxH :: e -> f -> Max e f

{- Showing a Set -}

instance Show (Set '[]) where
    show Empty = "{}"
instance (Show e, Show' (Set s)) => Show (Set (e ': s)) where
    show (Ext e s) = "{" ++ show e ++ (show' s) ++ "}" 

class Show' t where
    show' :: t -> String
instance Show' (Set '[]) where
    show' Empty = ""
instance (Show' (Set s), Show e) => Show' (Set (e ': s)) where
    show' (Ext e s) = ", " ++ show e ++ (show' s)

-- Split operation (with type level version)

class Split s t st where
   split :: Set st -> (Set s, Set t)

instance Split '[] '[] '[] where
   split Empty = (Empty, Empty)

instance Split s t st => Split (x ': s) (x ': t) (x ': st) where
   split (Ext x st) = let (s, t) = split st
                      in (Ext x s, Ext x t)

instance Split s t st => Split (x ': s) t (x ': st) where
   split (Ext x st) = let (s, t) = split st
                      in  (Ext x s, t) 

instance Split s t st => Split s (x ': t) (x ': st) where
   split (Ext x st) = let (s, t) = split st
                      in  (s, Ext x t) 

