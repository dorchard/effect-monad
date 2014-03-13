{-# LANGUAGE RebindableSyntax, GADTs, TypeFamilies, UndecidableInstances #-}

import Prelude hiding (Monad(..))

import Control.IxMonad
import ArrayReader

-- fooSym :: (Num a) => StencilM (Symmetrical (S Z)) a a
localMean = do a <- ix (Pos Z)
               b <- ix (Pos (S Z))
               c <- ix (Neg (S Z))
               return $ (a + b + c) / 3.0

fooFwd' :: Num a => Stencil (Forward (S Z)) a a
fooFwd' = Stencil $ do a <- ix (Pos Z)
                       b <- ix (Pos (S (S Z)))
                       return $ a + b 

fooFwd :: Num a => Stencil (Forward (S (S Z))) a a
fooFwd = Stencil $ do a <- ix (Pos Z)
                      b <- ix (Pos (S Z))
                      c <- ix (Pos (S (S Z)))
                      return $ a + b + c

 {-
--  The following causes a type error as it violates the specification
--  of symmetry

fooSymBroken :: (Num a) => StencilM a (Symmetrical (S Z)) a a
fooSymBroken = StencilM $ do a <- ix (Pos Z)
                             b <- ix (Pos (S Z))
                             return $ a + b 
-}

-- fooFwd has a 'forward' pattern to depth of 2


data Stencil r x y where
    Stencil :: (ArrayReader x spec y) -> Stencil (Sort spec) x y

-- Specification definitions

-- Forward-oriented stencil specification

type Forward sten = Sort (HCons Z (ForwardP sten))

-- ForwardP excludes the zero point
type family ForwardP depth 
type instance ForwardP Z     = HNil
type instance ForwardP (S n) = HCons (S n) (ForwardP n)

-- Symmetrical stencils (derived from Forward and Backward stencils of the same depth)

type Symmetrical depth = Sort (HCons Z (Append (ForwardP depth) (BackwardP depth)))

-- Backward-oriented stencils

type Backward sten = Sort (HCons Z (BackwardP sten))

type family BackwardP depth
type instance BackwardP Z     = HNil
type instance BackwardP (S n) = HCons (Neg (S n)) (BackwardP n)  

-- List sorting uses bubble sort (since this is easy to define inductively and for
-- the type system to handle!)

type Sort l = BSort l l

-- N-passes of bubble for a list of length N 

type family BSort l l'
type instance BSort l HNil = l
type instance BSort l (HCons x y) = BSort (Bubble l) y

-- Type-level sorting

type family Fst t
type instance Fst (a, b) = a

type family Snd t 
type instance Snd (a, b) = b

-- Single pass of bubble sort

type family Bubble l
type instance Bubble (HCons a HNil) = HCons a HNil
type instance Bubble (HCons a (HCons b c)) = HCons (Fst (SortLeft a b))
                                              (Bubble (HCons (Snd (SortLeft a b)) c))

type SortLeft n m = SortLeft' n m n m  

type family SortLeft' n m p q 

type instance SortLeft' Z Z p q = (p, q)
type instance SortLeft' Z (S m) p q = (p, q)
type instance SortLeft' (S m) Z p q = (q, p)
type instance SortLeft' (S m) (S n) p q = SortLeft' m n p q

type instance SortLeft' Z (Neg m) p q = (q, p)
type instance SortLeft' (S m) (Neg n) p q = (q, p)
type instance SortLeft' (Neg m) Z p q = (p, q)
type instance SortLeft' (Neg m) (S n) p q = (p, q)
type instance SortLeft' (Neg (S m)) (Neg (S n)) p q = SortLeft' (Neg m) (Neg n) p q