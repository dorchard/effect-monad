{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators, TypeFamilies, 
             MultiParamTypeClasses, FlexibleInstances, PolyKinds, FlexibleContexts,
             UndecidableInstances, IncoherentInstances, ConstraintKinds #-}

module Control.IxMonad.Helpers.Set (Set(..), Union, Unionable, union, bsort, append, Sort, Sortable, 
                                    Nubable(..), OrdH(..), Min, Max, Append(..), Split(..), 
                                    AsSet, asSet, IsSet, 
                                    Subset(..)) where

{- Core Set definition, in terms of lists -}

data Set (n :: [*]) where
    Empty :: Set '[]
    Ext :: e -> Set s -> Set (e ': s)

asSet :: (Sortable s, Nubable (Sort s)) => Set s -> Set (AsSet s)
asSet x = nub (bsort x)

type AsSet s = Nub (Sort s)

type IsSet s = (s ~ Nub (Sort s))

type SetProperties f = (Union f '[] ~ f, Split f '[] f, 
                        Union '[] f ~ f, Split '[] f f, 
                        Union f f ~ f, Split f f f,
                        Unionable f '[], Unionable '[] f)

{-- Union --}
type Union s t = Nub (Sort (Append s t))

union :: (Unionable s t) => Set s -> Set t -> Set (Union s t)
union s t = nub (bsort (append s t))

type Unionable s t = (Sortable (Append s t), Nubable (Sort (Append s t)))
type Sortable s = Bubbler s s

{- List append (essentially set disjoint union) -}

type family Append s t where
            Append '[] t = t
            Append (x ': xs) ys = x ': (Append xs ys)

append :: Set s -> Set t -> Set (Append s t)
append Empty x = x
append (Ext e xs) ys = Ext e (append xs ys)

{- Remove duplicates-}

type family Nub t where
    Nub '[]           = '[]
    Nub '[e]          = '[e]
    Nub (e ': e ': s) = Nub (e ': s)
    Nub (e ': f ': s) = e ': Nub (f ': s)

class Nubable t where
    nub :: Set t -> Set (Nub t)

instance Nubable '[] where
    nub Empty = Empty

instance Nubable '[e] where
    nub (Ext x Empty) = Ext x Empty

-- The case for equal types is not define here, but should be given
-- per-application

instance (Nub (e ': f ': s) ~ (e ': Nub (f ': s)), 
              Nubable (f ': s)) => Nubable (e ': f ': s) where
    nub (Ext e (Ext f s)) = Ext e (nub (Ext f s))

{- Sorting for normalising the representation -}

{- Sort top level -}
type Sort l = Bubble l l

bsort :: (Bubbler s s) => Set s -> Set (Sort s)
bsort x = bubble x x

{- Iteration of the buble sort -}
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
type family Pass l where
    Pass '[]           = '[]
    Pass '[e]          = '[e]
    Pass (e ': f ': s) = Min e f ': (Pass ((Max e f) ': s))

class Passer s where
    pass :: Set s -> Set (Pass s)

instance Passer '[] where
    pass Empty = Empty

instance Passer '[e] where
    pass (Ext e Empty) = Ext e Empty

instance (Passer ((Max e f) ': s), OrdH e f) => Passer (e ': f ': s) where 
    pass (Ext e (Ext f s)) = Ext (minH e f) (pass (Ext (maxH e f) s))

{- Ordering for the sort -}
type family Min a b
type family Max a b 

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

{- Split a set, given the sets we want to split it into -}
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

{-- Construct a subsetset 's' from a superset 'st' -}
class Subset s t where
   subset :: Set t -> Set s

instance Subset '[] t where 
   subset xs = Empty

instance Subset s t => Subset (x ': s) (x ': t) where
   subset (Ext x xs) = Ext x (subset xs)

instance Subset s t => Subset s (x ': t) where
   subset (Ext _ xs) = subset xs
