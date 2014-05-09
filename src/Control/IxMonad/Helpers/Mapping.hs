{-# LANGUAGE TypeOperators, DataKinds, FlexibleContexts, KindSignatures, PolyKinds #-}

module Control.IxMonad.Helpers.Mapping where

import GHC.TypeLits

data (k :: Symbol) :-> (v :: *) = (Var k) :->  v
data Var (k :: Symbol) = Var

instance (Show (Var k), Show v) => Show (k :-> v) where
    show (k :-> v) = "(" ++ show k ++ ", " ++ show v ++ ")"