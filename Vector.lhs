> {-# LANGUAGE TypeFamilies, EmptyDataDecls, ExistentialQuantification, GADTs, 
>      MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, DataKinds, 
>       TypeOperators, PolyKinds #-}

> import IxComonad

> import GHC.TypeLits

> data HNil 
> data HCons (n :: m) (ns :: m)

 data HList t where
     HNil :: HList HNil
     HNil 



> data Nat1 = Z | S Nat1
 


 data Z
 data S n

 data Nat n where
     Z :: Nat Z
     S :: Nat n -> Nat (S n)

> data Vector (n :: Nat1) a where
>     Nil :: Vector Z a
>     Cons :: a -> Vector n a -> Vector (S n) a

Prefix (S Z) n => 

 class LookupV n m where
     lookupV :: Vector n a -> m -> a

 instance LookupV Z m where
     lookupV Nil _ = error "Oops"

 instance LookupV (S n) Z where
     lookupV (Cons x xs) Z = x

 instance LookupV n m => LookupV (S n) (S m) where
     lookupV (Cons x xs) (S n) = lookupV xs n 

> data PVector n a = PV a (Vector n a) 



> instance IxComonad PVector where
>     type Unit PVector = Z
>     type Plus PVector Z n = n
>     type Plus PVector (S m) n = S (Plus Vector m n)

>     iextract (PV x Nil Z) = x

     iextend k vs = 

> class PVectorExtend s t where
>     pvextend :: (PVector s a -> b) -> PVector (PlusA s t) a -> PVector t b
>     pvextend k (PV x xs n) = let (s, t) = prefiyx xs
>                              in indices' (
                    in k x

 type family IndicesT (n :: Nat1)
 type instance IndicesT Z = HNil
 type instance IndicesT (S n) = HCons (S n) (IndicesT n)


> class Indices n where
>     indices' :: PVector n a -> Int -> [Int] 

     indices :: PVector n a -> IndicesT n

> instance Indices Z where
>     indices' (PV _ Nil _) _ = []

     indices Z = HNil

> instance Indices n => Indices (S n) where
>     indices' (PV _ (Cons x xs)) n = n : (indices' (PV x xs) (n + 1))

     indices (S n) = HCons (S n) (indices n)

> peel :: Vector (S n) a -> a
> peel (Cons x xs) = x

> peel2 :: Vector (S (S n)) a -> Vector (S (S Z)) a
> peel2 (Cons x (Cons y ys)) = (Cons x (Cons y Nil))

> tail :: Vector (S t) a -> Vector t a
> tail (Cons x xs) = xs

> data VectorA (n :: Nat) a where
>     NilA :: VectorA 0 a
>     ConsA :: a -> VectorA n a -> VectorA (n + 1) a

> type family PlusA (n :: Nat1) (m :: Nat1) :: Nat1
> type instance PlusA Z n = n
> type instance PlusA (S n) m = S (PlusA n m)


 class Prefix s t where
     prefix :: Vector (Plus PVector s t) a -> (Vector s a, Vector t a)

> class Prefix s t where
>     prefix :: Vector (PlusA s t) a -> (Vector s a, Vector t a)


> instance Prefix Z t where
>     prefix left = (Nil, left)

> instance Prefix s t => Prefix (S s) t where
>    prefix (Cons x xs) = let (s', t') = prefix xs
>                         in (Cons x s', t')

    prefix (S n) (Cons x xs) = Cons x (prefix n xs)



 instance PVectorExtend s t where
     pvextend k = 