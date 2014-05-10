{-# LANGUAGE TypeOperators, DataKinds, FlexibleContexts, KindSignatures, PolyKinds, 
  ScopedTypeVariables, TypeFamilies, GADTs, MultiParamTypeClasses, UndecidableInstances #-}

module Control.IxMonad.Helpers.Mapping where

import Control.IxMonad.Helpers.Set
import GHC.TypeLits
import Data.Proxy

infixl 2 :->
data (k :: Symbol) :-> (v :: *) = (Var k) :->  v

data Var (k :: Symbol) where Var :: Var k 
                             X   :: Var "x"
                             Y   :: Var "y"
                             Z   :: Var "z"

instance (Show (Var k), Show v) => Show (k :-> v) where
    show (k :-> v) = "(" ++ show k ++ ", " ++ show v ++ ")"

-- Selection based on comparing symbols 

type Select a b p q = Choose (CmpSymbol a b) p q

class Chooser (o :: Ordering) where
    type Choose (o :: Ordering) (p :: k) (q :: k) :: k
    choose :: (Proxy o) -> p -> q -> (Choose o p q)

instance Chooser LT where
    type Choose LT p q = p
    choose _ p q = p

instance Chooser EQ where
    type Choose EQ p q = p
    choose _ p q = p

instance Chooser GT where
    type Choose GT p q = q
    choose _ p q = q

select :: forall j k a b . (Chooser (CmpSymbol j k)) => 
          Var j -> Var k -> a -> b -> Select j k a b
select _ _ x y = choose (Proxy::(Proxy (CmpSymbol j k))) x y 


type instance Min (j :-> u) (k :-> v) = (Select j k j k) :-> (Select j k u v)
type instance Max (j :-> u) (k :-> v) = (Select j k k j) :-> (Select j k v u)

instance (Chooser (CmpSymbol j k)) => OrdH (j :-> u) (k :-> v) where
    minH (j :-> u) (k :-> v) = Var :-> (select j k u v)
    maxH (j :-> u) (k :-> v) = Var :-> (select j k v u)

