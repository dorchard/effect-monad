{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators, TypeFamilies, FlexibleContexts, 
             MultiParamTypeClasses, FlexibleInstances, PolyKinds, KindSignatures #-}

module Control.Effect.Helpers.List where

import Data.Proxy 

data List (l::[*]) where
    Nil   :: List '[]
    Cons  :: x -> List xs -> List (x ': xs)

type family (:++) (s :: [*]) (t :: [*]) :: [*] where
    '[] :++ ys       = ys
    (x ': xs) :++ ys = x ': (xs :++ ys)

append :: List s -> List t -> List (s :++ t)
append Nil xs = xs
append (Cons x xs) ys = Cons x (append xs ys)

class Split (s :: [*]) (t :: [*]) where
    split :: List (s :++ t) -> (List s, List t)

instance Split '[] xs where
    split xs = (Nil, xs)

instance Split xs ys => Split (x ': xs) ys where
    split (Cons x xs) = let (xs', ys') = split xs
                    in (Cons x xs', ys')

lengthL :: List xs -> Int
lengthL Nil = 0
lengthL (Cons _ xs) = 1 + lengthL xs

type family LookupT  k xs where
    LookupT k '[] = Maybe ()
    LookupT k ((k , x) ': xs) = Maybe x
    LookupT k ((j , x) ': xs) = LookupT k xs

class LookUpA k xs (LookupT k xs) => Lookup k xs where
    lookup :: List xs -> Proxy k -> Maybe (LookupT k xs) 
    lookup = lookupA

class LookUpA k xs x where
    lookupA :: List xs -> Proxy k -> Maybe x

instance LookUpA k ((k, x) ': xs) x where
    lookupA (Cons (_, x) _) k = Just x 

instance LookUpA k xs y => LookUpA k ((j, x) ': xs) y where
    lookupA (Cons _ xs) k = lookupA xs k


