{-# LANGUAGE RebindableSyntax, GADTs, TypeFamilies, DataKinds, TypeOperators, UndecidableInstances #-}

import Prelude hiding (Monad(..))

import GHC.TypeLits hiding (Nat)
import Data.Type.Set
import Control.Effect
import ArrayReader

localMean :: (Num a, Fractional a) => Stencil a (Symmetrical 1) a
--localMean :: (Num a, Fractional a) => Stencil a '[IntT (Neg 1), IntT (Pos 0), IntT (Pos 1)] a
localMean = do a <- ix (IntT :: (IntT (Pos 0)))
               b <- ix (IntT :: (IntT (Pos 1)))
               c <- ix (IntT :: (IntT (Neg 1)))
               return $ (a + b + c) / 3.0

fooFwd :: Num a => Stencil a (Forward 2) a
fooFwd = do a <- ix (IntT :: (IntT (Pos 0)))
            b <- ix (IntT :: (IntT (Pos 1)))
            c <- ix (IntT :: (IntT (Pos 2)))
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

-- Specification definitions

-- Forward-oriented stencil specification

type Forward sten = AsSet ((IntT (Pos 0)) ': (ForwardP sten))

-- ForwardP excludes the zero point
type family ForwardP depth where
            ForwardP 0 = '[]
            ForwardP n = (IntT (Pos n)) ': (ForwardP (n - 1))


-- Symmetrical stencils (derived from Forward and Backward stencils of the same depth)

type Symmetrical depth = AsSet ((IntT (Pos 0)) ': ((ForwardP depth) :++ (BackwardP depth)))

-- Backward-oriented stencils

type Backward sten = AsSet ((IntT (Pos 0)) ': (BackwardP sten))

type family  BackwardP depth where
             BackwardP 0 = '[]
             BackwardP n = (IntT (Neg n)) ': (BackwardP (n - 1))
