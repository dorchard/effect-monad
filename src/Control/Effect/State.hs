{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, GADTs, 
             EmptyDataDecls, UndecidableInstances, RebindableSyntax, OverlappingInstances, 
             DataKinds, TypeOperators, PolyKinds, NoMonomorphismRestriction, FlexibleContexts,
             AllowAmbiguousTypes, ScopedTypeVariables, FunctionalDependencies, ConstraintKinds, 
             InstanceSigs, IncoherentInstances #-}

module Control.Effect.State (Set(..), get, put, State(..), (:->)(..), (:!)(..),
                                  Eff(..), Action(..), Var(..), union, UnionS, 
                                     Reads(..), Writes(..), Unionable, Sortable, SetLike, 
                                      StateSet, 
                                          --- may not want to export these
                                          IntersectR, Update, Sort, Split) where

import Control.Effect
import Control.Effect.Helpers.Mapping 
import Control.Effect.Helpers.Set hiding (Unionable, union, SetLike, Nub, Nubable(..))
import qualified Control.Effect.Helpers.Set as Set

import Prelude hiding (Monad(..),reads)
import GHC.TypeLits
import Data.Proxy
import Debug.Trace

-- Distinguish reads, writes, and read-writes
data Eff = R | W | RW

data Action (s :: Eff) = Eff

instance Show (Action R) where
    show _ = "R"
instance Show (Action W) where
    show _ = "W"
instance Show (Action RW) where
    show _ = "RW"

data (:!) (a :: *) (s :: Eff) = a :! (Action s) 

instance (Show (Action f), Show a) => Show (a :! f) where
    show (a :! f) = show a ++ " ! " ++ show f

infixl 3 :!

type SetLike s = Nub (Sort s)
type UnionS s t = Nub (Sort (Append s t))
type Unionable s t = (Sortable (Append s t), Nubable (Sort (Append s t)) (Nub (Sort (Append s t))),
                      Split s t (Union s t))

union :: (Unionable s t) => Set s -> Set t -> Set (UnionS s t)
union s t = nub (bsort (append s t))

{- Remove duplicates from a type-level list and turn different sorts into 'RW' -}

type family Nub t where
    Nub '[]       = '[]
    Nub '[e]      = '[e]
    Nub (e ': e ': as) = Nub (e ': as)
    Nub ((k :-> a :! s) ': (k :-> a :! t) ': as) = Nub ((k :-> a :! RW) ': as)
    Nub (e ': f ': as) = e ': Nub (f ': as)

class Nubable t v where
    nub :: Set t -> Set v

instance Nubable '[] '[] where
    nub Empty = Empty

instance Nubable '[e] '[e] where
    nub (Ext e Empty) = (Ext e Empty)

instance Nubable ((k :-> b :! s) ': as) as' => 
    Nubable ((k :-> a :! s) ': (k :-> b :! s) ': as) as' where
    nub (Ext _ (Ext x xs)) = nub (Ext x xs)

instance Nubable ((k :-> a :! RW) ': as) as' => 
    Nubable ((k :-> a :! s) ': (k :-> a :! t) ': as) as' where
    nub (Ext _ (Ext (k :-> (a :! _)) xs)) = nub (Ext (k :-> (a :! (Eff::(Action RW)))) xs)

instance Nubable ((j :-> b :! t) ': as) as' => 
    Nubable ((k :-> a :! s) ': (j :-> b :! t) ': as) ((k :-> a :! s) ': as') where
    nub (Ext (k :-> (a :! s)) (Ext (j :-> (b :! t)) xs)) = Ext (k :-> (a :! s)) (nub (Ext (j :-> (b :! t)) xs))


{- Update reads, that is any writes are pushed into reads, a bit like intersection -}

class Update t v where
    update :: Set t -> Set v

instance Update xs '[] where
    update _ = Empty

instance Update '[e] '[e] where 
    update s = s


instance Update ((k :-> b :! R) ': as) as' => Update ((k :-> a :! s) ': (k :-> b :! s) ': as) as' where
    update (Ext _ (Ext (k :-> (b :! _)) xs)) = update (Ext (k :-> (b :! (Eff::(Action R)))) xs) 

instance Update ((k :-> a :! R) ': as) as' => Update ((k :-> a :! W) ': (k :-> b :! R) ': as) as' where
    update (Ext (k :-> (a :! _)) (Ext _ xs)) = update (Ext (k :-> (a :! (Eff::(Action R)))) xs)


instance Update ((j :-> b :! s) ': as) as' => Update ((k :-> a :! W) ': (j :-> b :! s) ': as) as' where
    update (Ext _ (Ext e xs)) = update (Ext e xs)

instance Update ((j :-> b :! s) ': as) as' => Update ((k :-> a :! R) ': (j :-> b :! s) ': as) ((k :-> a :! R) ': as') where
    update (Ext e (Ext e' xs)) = Ext e $ update (Ext e' xs)

type IntersectR s t = (Sortable (Append s t), Update (Sort (Append s t)) t)

intersectR :: (Reads t ~ t, Writes s ~ s, IsSet s, IsSet t, IntersectR s t) => Set s -> Set t -> Set t
intersectR s t = update (bsort (append s t))

-- Effect-parameterised state type

data State s a = State { runState :: Set (Reads s) -> (a, Set (Writes s)) }

type family Reads t where
    Reads '[]                    = '[]
    Reads ((k :-> a :! R) ': xs)  = (k :-> a :! R) ': (Reads xs)
    Reads ((k :-> a :! RW) ': xs) = (k :-> a :! R) ': (Reads xs)
    Reads ((k :-> a :! W) ': xs)  = Reads xs

type family Writes t where
    Writes '[]                     = '[]
    Writes ((k :-> a :! W) ': xs)  = (k :-> a :! W) ': (Writes xs)
    Writes ((k :-> a :! RW) ': xs) = (k :-> a :! W) ': (Writes xs)
    Writes ((k :-> a :! R) ': xs)  = Writes xs

-- 'get/put' monadic primitives

get :: Var k -> State '[k :-> a :! R] a
get _ = State $ \(Ext (k :-> (a :! _)) Empty) -> (a, Empty)

put :: Var k -> a -> State '[k :-> a :! W] ()
put _ a = State $ \Empty -> ((), Ext (Var :-> a :! Eff) Empty)


type StateSet f = (StateSetProperties f, StateSetProperties (Reads f), StateSetProperties (Writes f))
                   
type StateSetProperties f = (IntersectR f '[], IntersectR '[] f,
                             UnionS f '[] ~ f, Split f '[] f, 
                             UnionS '[] f ~ f, Split '[] f f, 
                             UnionS f f ~ f, Split f f f,
                             Unionable f '[], Unionable '[] f)
                   
-- Indexed monad instance
instance Effect State where
    type Inv State s t = (IsSet s, IsSet (Reads s), IsSet (Writes s),
                          IsSet t, IsSet (Reads t), IsSet (Writes t),
                          Reads (Reads t) ~ Reads t, Writes (Writes s) ~ Writes s, 
                            Split (Reads s) (Reads t) (Reads (UnionS s t)), 
                            Unionable (Writes s) (Writes t), 
                            IntersectR (Writes s) (Reads t), 
                            Writes (UnionS s t) ~ UnionS (Writes s) (Writes t))
    type Unit State = '[]
    type Plus State s t = UnionS s t

    return x = State $ \Empty -> (x, Empty)

    (State e) >>= k = 
        State $ \st -> let (sR, tR) = split st
                           (a, sW)  = e sR
                           (b, tW) = (runState $ k a) (sW `intersectR` tR)
                       in  (b, sW `union` tW) 


{-
instance (Split s t (Union s t), Sub s t) => Subeffect State s t where
    sub (State e) = IxR $ \st -> let (s, t) = split st 
                                           _ = ReflP p t 
                                 in e s
-}

