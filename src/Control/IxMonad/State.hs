{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, GADTs, 
             EmptyDataDecls, UndecidableInstances, RebindableSyntax, OverlappingInstances, 
             DataKinds, TypeOperators, PolyKinds #-}

module Control.IxMonad.State where

import Control.IxMonad
import Prelude hiding (Monad(..))
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


-- Type-level bubble sort on list
type family Bubble l where
            Bubble Nil                       = Nil
            Bubble (Cons k s a Nil)          = Cons k s a Nil
            Bubble (Cons j s a (Cons k t b c)) = 
                       Cons (MinKey j k j k)  (MinKey j k s t) (MinKey j k a b)
                           (Bubble (Cons (MaxKey j k j k) (MaxKey j k s t) (MaxKey j k a b) c))

-- Return the minimum or maximum of two types which consistitue key-value pairs
type MinKey (a :: Symbol) (b :: Symbol) (p :: k) (q :: k) = Choose (CmpSymbol a b) p q
type MaxKey (a :: Symbol) (b :: Symbol) (p :: k) (q :: k) = Choose (CmpSymbol a b) q p

type family Choose (g :: Ordering) a b where
    Choose LT p q = p
    Choose EQ p q = p
    Choose GT p q = q


-- Indexed state type

data IxState s a = IxS { unIxS :: List (LHS s) -> (a, List (RHS s)) }

type family LHS t where
    LHS Nil = Nil
    LHS (Cons k R a xs) = Cons k R a (LHS xs)
    LHS (Cons k W a xs) = LHS xs
    LHS (Cons k U a xs) = Cons k U a (LHS xs)
type family RHS t where
    RHS Nil = Nil
    RHS (Cons k R a xs) = RHS xs
    RHS (Cons k W a xs) = Cons k W a (RHS xs)
    RHS (Cons k U a xs) = Cons k U a (RHS xs)

-- 'ask' monadic primitive

get :: Proxy (k::Symbol) -> IxState (Cons k R a Nil) a
get Proxy = IxS $ \(Cons Proxy Proxy a Nil) -> (a, Nil)

put :: Proxy (k::Symbol) -> a -> IxState (Cons k W a Nil) a
put Proxy a = IxS $ \Nil -> (a, Cons Proxy Proxy a Nil)


{-

-- Indexed monad instance

instance IxMonad IxState where
    type Inv IxReader s t = Split s t (Union s t)

    type Unit IxState = Nil
    type Plus IxState s t = Union s t

    return x = IxS $ \Nil -> x

    (IxR e) >>= k = IxR $ \xs -> let (s, t) = split xs
                                     (a, st') = unIxR $ e s
                                     (b, st'') = k a t
                                 in (b, st' `append` st'')




-- Split operation (with type level version)

class Split s t z where
   split :: List z -> (List s, List t)

instance Split Nil Nil Nil where
   split Nil = (Nil, Nil) 

instance Split (Cons k x xs) Nil (Cons k x xs) where
    split t = (t, Nil)

instance Split Nil (Cons k x xs) (Cons k x xs) where
   split t = (Nil, t)

instance Split (Cons k x Nil) (Cons k x Nil) (Cons k x Nil) where
   split (Cons Proxy x Nil) = (Cons Proxy x Nil, Cons Proxy x Nil)

instance Split xs ys zs => Split (Cons k x xs) (Cons k x ys) (Cons k x zs) where
   split (Cons k x zs) = let (xs', ys') = split zs
                         in (Cons k x xs', Cons k x ys')

instance (Split xs ys zs) => Split (Cons j x xs) (Cons k y ys) (Cons j x (Cons k y zs)) where
   split (Cons j x (Cons k y zs)) = let (xs', ys') = split zs
                                    in (Cons j x xs', Cons k y ys')

instance (Split xs ys zs) => Split (Cons j x xs) (Cons k y ys) (Cons k y (Cons j x zs)) where
   split (Cons j x (Cons k y zs)) = let (xs', ys') = split zs
                                    in (Cons k y xs', Cons j x ys')

foo :: IxReader (Cons "x" a (Cons "xs" [a] Nil)) [a]
foo = do x <- ask (Proxy::(Proxy "x"))
         xs <- ask (Proxy::(Proxy "xs"))
         return (x:xs)
-}