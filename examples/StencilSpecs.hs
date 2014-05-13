{-# LANGUAGE RebindableSyntax, GADTs, TypeFamilies, DataKinds, TypeOperators #-}

import Prelude hiding (Monad(..))

import GHC.TypeLits hiding (Nat)
import Control.Effect.Helpers.Set
import Control.Effect
import ArrayReader

localMean :: (Num a, Fractional a) => Stencil (Symmetrical (S Z)) a a
localMean = Stencil $ 
             do a <- ix (Pos Z)
                b <- ix (Pos (S Z))
                c <- ix (Neg (S Z))
                return $ (a + b + c) / 3.0


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

type Forward sten = AsSet (Z ': (ForwardP sten))

-- ForwardP excludes the zero point
type family ForwardP depth where
            ForwardP Z     = '[]
            ForwardP (S n) = (S n) ': (ForwardP n)

-- Symmetrical stencils (derived from Forward and Backward stencils of the same depth)

type Symmetrical depth = AsSet (Z ': (Append (ForwardP depth) (BackwardP depth)))

-- Backward-oriented stencils

type Backward sten = AsSet (Z ': (BackwardP sten))

type family  BackwardP depth where
             BackwardP Z     = '[]
             BackwardP (S n) = (Neg (S n)) ': (BackwardP n)  
