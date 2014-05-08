{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators, TypeFamilies, 
             MultiParamTypeClasses, FlexibleInstances #-}

module Control.IxMonad.Helpers.List where

data List (l::[*]) where
    Nil   :: List '[]
    Cons  :: x -> List xs -> List (x ': xs)

type family (:++) (s :: [*]) (t :: [*]) :: [*]
type instance '[] :++ ys       = ys
type instance (x ': xs) :++ ys = x ': (xs :++ ys)

append :: List s -> List t -> List (s :++ t)
append Nil xs = xs
append (Cons x xs) ys = Cons x (append xs ys)

class Split s t where
    split :: List (s :++ t) -> (List s, List t)

instance Split '[] xs where
    split xs = (Nil, xs)

instance Split xs ys => Split (x ': xs) ys where
    split (Cons x xs) = let (xs', ys') = split xs
                    in (Cons x xs', ys')