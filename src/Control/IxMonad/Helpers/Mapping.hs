{-# LANGUAGE TypeOperators, DataKinds, FlexibleContexts, KindSignatures, PolyKinds, 
  ScopedTypeVariables, TypeFamilies, GADTs, MultiParamTypeClasses, UndecidableInstances, 
  FlexibleInstances #-}

module Control.IxMonad.Helpers.Mapping where

import Control.IxMonad.Helpers.Set
import GHC.TypeLits
import Data.Proxy

infixl 2 :->
data (k :: Symbol) :-> (v :: *) = (Var k) :-> v

data Var (k :: Symbol) where Var :: Var k 
                             X   :: Var "x"
                             Y   :: Var "y"
                             Z   :: Var "z"

instance (Show (Var k), Show v) => Show (k :-> v) where
    show (k :-> v) = "(" ++ show k ++ ", " ++ show v ++ ")"

instance Show (Var "x") where
    show _ = "x"

instance Show (Var "y") where
    show _ = "y"

instance Show (Var "z") where
    show _ = "z"

select :: forall j k a b . (Chooser (CmpSymbol j k)) => 
          Var j -> Var k -> a -> b -> Select j k a b
select _ _ x y = choose (Proxy::(Proxy (CmpSymbol j k))) x y 

instance (Chooser (CmpSymbol j k)) => OrdH (j :-> u) (k :-> v) where
    minH (j :-> u) (k :-> v) = Var :-> (select j k u v)
    maxH (j :-> u) (k :-> v) = Var :-> (select j k v u)

type instance Min (j :-> u) (k :-> v) = (Select j k j k) :-> (Select j k u v)
type instance Max (j :-> u) (k :-> v) = (Select j k k j) :-> (Select j k v u)

type Select a b p q = Choose (CmpSymbol a b) p q

type family Choose (o :: Ordering) p q where
            Choose LT p q = p
            Choose EQ p q = p
            Choose GT p q = q

class Chooser (o :: Ordering) where
    choose :: (Proxy o) -> p -> q -> (Choose o p q)

instance Chooser LT where
    choose _ p q = p

instance Chooser EQ where
    choose _ p q = p

instance Chooser GT where
    choose _ p q = q