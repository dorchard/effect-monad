> {-# LANGUAGE TypeFamilies, EmptyDataDecls, ExistentialQuantification, GADTs, 
>      MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, DataKinds, 
>       TypeOperators, PolyKinds, UndecidableInstances, FunctionalDependencies #-}

> import IxComonad

> import GHC.TypeLits

> data HNil 
> data HCons (n :: m) (ns :: m)

 data HList t where
     HNil :: HList HNil
     HNil 



> data Nat1 = Z | S Nat1
 
> data NatRep (n :: Nat1) where
>     NZ :: NatRep Z
>     NS :: NatRep n -> NatRep (S n)

 data Z
 data S n

 data Nat n where
     Z :: Nat Z
     S :: Nat n -> Nat (S n)

> data Vector (n :: Nat1) a where
>     Nil :: Vector Z a
>     Cons :: a -> Vector n a -> Vector (S n) a

> instance Functor (Vector Z) where
>     fmap f Nil = Nil
> instance Functor (Vector n) => Functor (Vector (S n)) where
>     fmap f (Cons x xs) = Cons (f x) (fmap f xs)

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

> instance (Functor (Vector n)) => Functor (PVector n) where
>     fmap f (PV a xs) = PV (f a) (fmap f xs)

> -- Smart constructor
> pv :: Vector (S n) a -> PVector n a
> pv (Cons x xs) = PV x xs

> instance IxComonad PVector where
>     type Unit PVector = Z
>     type Plus PVector Z n = n
>     type Plus PVector (S m) n = S (PlusA m n)

>     iextract (PV x Nil) = x

     iextend k vs = 

 class PVectorExtend s t where
     pvextend :: (PVector s a -> b) -> PVector (PlusA s t) a -> PVector t b
     pvextend k (PV x xs n) = let (s, t) = prefix xs
                              in 
                    in k x

 type family IndicesT (n :: Nat1)
 type instance IndicesT Z = HNil
 type instance IndicesT (S n) = HCons (S n) (IndicesT n)


 class Indices n where
     indices' :: PVector n a -> Int -> [Int] 
 instance Indices Z where
     indices' (PV _ Nil _) _ = []
 instance Indices n => Indices (S n) where
     indices' (PV _ (Cons x xs)) n = n : (indices' (PV x xs) (n + 1))


> type family PlusA (n :: Nat1) (m :: Nat1) :: Nat1
> type instance PlusA Z n = n
> type instance PlusA (S n) m = S (PlusA n m)

 type family F2 a where                           -- OK!
   F (Maybe Int)  = Int
   F (Maybe Bool) = Bool
   F (Maybe a)    = String

 type instance PlusA (S n) Z = S n
 type instance PlusA Z (S m) = (S m)
 type instance PlusA (S n) (S m) = S (S (PlusA n m))


 class Prefix s t where
     prefix :: Vector (Plus PVector s t) a -> (Vector s a, Vector t a)

 class CojoinV s t where
     cojoinV :: Vector (PlusA s t) a -> Vector s (Vector t a)

 instance CojoinV Z Z where
     cojoinV Nil = Nil

 instance CojoinV n m => CojoinV (S n) m where
     cojoinV (Cons x xs) = let (ys, zs) = prefix (Cons x xs)
                           in Cons ys (cojoinV xs)


> class Cojoin s t  where
>     cojoin :: PVector (PlusA s t) a -> PVector s (PVector t a)

>     prefixPV :: PVector (PlusA s t) a -> (PVector s a, Vector t a)

>     prefixPVn :: PVector (PlusA (S s) t) a -> (PVector (S s) a, PVector (PlusA s t) a)


> instance Cojoin Z Z where
>    cojoin (PV a Nil) = PV (PV a Nil) Nil

>    prefixPV (PV a Nil) = (PV a Nil, Nil)

>    prefixPVn (PV a (Cons x Nil)) = (PV a (Cons x Nil), PV x Nil)
>                          

> instance Cojoin Z (S n) where
>    cojoin (PV a xs) = PV (PV a xs) Nil

>    prefixPV (PV a xs) = (PV a Nil, xs)

>    prefixPVn (PV a (Cons x xs)) = (PV a (Cons x Nil), PV x xs)

> instance Cojoin n m => Cojoin (S n) m where

    prefixPVn (PV a (Cons x (Cons x' xs))) 
                  = let (PV b ys, PV c zs) = prefixPVn (PV x' xs)
                    in (PV a (Cons x (Cons b ys)), PV x' (Cons c zs))

>    prefixPVn (PV a (Cons x xs)) = let (PV b ys, PV c zs) = prefixPVn (PV x xs)
>                                   in (PV a (Cons b ys), PV x (Cons c zs))

>    prefixPV (PV a (Cons x xs)) = let (PV b ys, zs) = prefixPV (PV x xs)
>                                  in (PV a (Cons b ys), zs)

    cojoin p@(PV a (Cons x xs)) = let (ys, zs) = prefixPV p
                                      (PV a' zs') = cojoin (PV x xs)
                                  in PV ys (Cons a' zs')



 instance Cojoin n m => Cojoin (S n) (S m) where

   prefixV (Cons x xs) = Cons x (prefixV xs)


 instance Cojoin 

 instance (Functor (Vector (S n))) => Cojoin (S n) Z where
    cojoin (PV a xs) = PV (PV a Nil) (fmap (\a -> PV a Nil) xs)

 instance Cojoin (S n) Z where
     cojoin (PV a xs) = PV (PV a xs) Nil


> class Prefix s t where
>     prefix :: Vector (PlusA s t) a -> (Vector s a, Vector t a)

> instance Prefix Z t where
>     prefix left = (Nil, left)

> instance Prefix s t => Prefix (S s) t where
>    prefix (Cons x xs) = let (s', t') = prefix xs
>                         in (Cons x s', t')

