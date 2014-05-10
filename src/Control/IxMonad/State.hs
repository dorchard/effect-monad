{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, GADTs, 
             EmptyDataDecls, UndecidableInstances, RebindableSyntax, OverlappingInstances, 
             DataKinds, TypeOperators, PolyKinds, NoMonomorphismRestriction, FlexibleContexts,
             AllowAmbiguousTypes, ScopedTypeVariables, FunctionalDependencies, ConstraintKinds, 
             InstanceSigs, IncoherentInstances #-}

module Control.IxMonad.State (Set(..), get, put, IxState(..), (:->)(..), (:!)(..),
                                  Eff(..), Effect(..), Var(..), union, UnionS, 
                                     Reads(..), Writes(..), Unionable, Sortable, Intersectable) where

import Control.IxMonad
import Control.IxMonad.Helpers.Mapping 
import Control.IxMonad.Helpers.Set hiding (Unionable, union)
import Prelude hiding (Monad(..),reads)
import GHC.TypeLits
import Data.Proxy
import Debug.Trace

-- Distinguish reads, writes, and read-writes
data Eff = R | W | RW

data Effect (s :: Eff) = Eff

instance Show (Effect R) where
    show _ = "R"
instance Show (Effect W) where
    show _ = "W"
instance Show (Effect RW) where
    show _ = "RW"

data (:!) (a :: *) (s :: Eff) = a :! (Effect s) 

instance (Show (Effect f), Show a) => Show (a :! f) where
    show (a :! f) = show a ++ " ! " ++ show f

infixl 3 :!

type UnionS s t = RemDupState (Sort (Append s t))
type Unionable s t = (Sortable (Append s t), RemDuperS (Sort (Append s t)) (RemDupState (Sort (Append s t))))

union :: (Unionable s t) => Set s -> Set t -> Set (UnionS s t)
union s t = remDupState (bsort (append s t))

-- Remove duplicates from a type-level list and turn different sorts into 'RW'
type family RemDupState t where
            RemDupState '[]       = '[]
            RemDupState '[e]      = '[e]
            RemDupState ((k :-> a :! s) ': (k :-> b :! s) ': as) = RemDupState ((k :-> b :! s) ': as)
            RemDupState ((k :-> a :! s) ': (k :-> a :! t) ': as) = RemDupState ((k :-> a :! RW) ': as)
            RemDupState ((k :-> a :! s) ': (j :-> b :! t) ': as) = (k :-> a :! s) ': RemDupState ((j :-> b :! t) ': as)


class RemDuperS t v where
    remDupState :: Set t -> Set v

instance RemDuperS '[] '[] where
    remDupState Empty = Empty

instance RemDuperS '[e] '[e] where
    remDupState (Ext e Empty) = (Ext e Empty)

instance RemDuperS ((k :-> b :! s) ': as) as' => 
          RemDuperS ((k :-> a :! s) ': (k :-> b :! s) ': as) as' where
    remDupState (Ext _ (Ext x xs)) = remDupState (Ext x xs)

instance RemDuperS ((k :-> a :! RW) ': as) as' => 
           RemDuperS ((k :-> a :! s) ': (k :-> a :! t) ': as) as' where
    remDupState (Ext _ (Ext (k :-> (a :! _)) xs)) = remDupState (Ext (k :-> (a :! (Eff::(Effect RW)))) xs)

instance RemDuperS ((j :-> b :! t) ': as) as' => 
             RemDuperS ((k :-> a :! s) ': (j :-> b :! t) ': as) ((k :-> a :! s) ': as') where
    remDupState (Ext (k :-> (a :! s)) (Ext (j :-> (b :! t)) xs)) = Ext (k :-> (a :! s)) (remDupState (Ext (j :-> (b :! t)) xs))


class UpdateReads t v where
    updateReads :: Set t -> Set v

instance UpdateReads '[] '[] where
    updateReads Empty = Empty

instance UpdateReads '[k :-> (a :! W)] '[] where
    updateReads (Ext e Empty) = Empty

instance UpdateReads '[e] '[e] where 
    updateReads (Ext e Empty) = Ext e Empty

instance UpdateReads ((k :-> b :! R) ': as) as' => UpdateReads ((k :-> a :! s) ': (k :-> b :! s) ': as) as' where
    updateReads (Ext _ (Ext (k :-> (b :! _)) xs)) = updateReads (Ext (k :-> (b :! (Eff::(Effect R)))) xs)

instance UpdateReads ((k :-> a :! R) ': as) as' => UpdateReads ((k :-> a :! W) ': (k :-> b :! R) ': as) as' where
    updateReads (Ext (k :-> (a :! _)) (Ext _ xs)) = updateReads (Ext (k :-> (a :! (Eff::(Effect R)))) xs)

instance UpdateReads ((k :-> b :! R) ': as) as' => UpdateReads ((k :-> a :! s) ': (k :-> b :! W) ': as) as' where
    updateReads (Ext _ (Ext (k :-> (b :! _)) xs)) = updateReads (Ext (k :-> (b :! (Eff::(Effect R)))) xs)

instance UpdateReads ((k :-> a :! R) ': as) as' => UpdateReads ((k :-> a :! RW) ': (k :-> a :! R) ': as) as' where
    updateReads (Ext (k :-> (a :! _)) (Ext _ xs)) = updateReads (Ext (k :-> (a :! (Eff::(Effect R)))) xs)

instance UpdateReads ((k :-> b :! R) ': as) as' => UpdateReads ((k :-> a :! R) ': (k :-> b :! RW) ': as) as' where
    updateReads (Ext _ (Ext (k :-> (b :! _)) xs)) = updateReads (Ext (k :-> (b :! (Eff::(Effect R)))) xs)

instance UpdateReads ((j :-> b :! s) ': as) as' => UpdateReads ((k :-> a :! W) ': (j :-> b :! s) ': as) as' where
    updateReads (Ext _ (Ext e xs)) = updateReads (Ext e xs)

instance UpdateReads ((j :-> b :! s) ': as) as' => UpdateReads ((k :-> a :! R) ': (j :-> b :! s) ': as) ((k :-> a :! R) ': as') where
    updateReads (Ext e (Ext e' xs)) = Ext e $ updateReads (Ext e' xs)

type Intersectable s t = (Sortable (Append s t), UpdateReads (Sort (Append s t)) t)

intersectReads :: (Sortable (Append s t), Intersectable s t) => Set s -> Set t -> Set t
intersectReads s t = updateReads (bsort (append s t))

-- Effect-parameterised state type

data IxState s a = IxS { runState :: Set (Reads s) -> (a, (Set (Writes s))) }

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

get :: Var (k::Symbol) -> IxState '[k :-> a :! R] a
get _ = IxS $ \(Ext (k :-> (a :! _)) Empty) -> (a, Empty)

put :: Var (k::Symbol) -> a -> IxState '[k :-> a :! W] ()
put _ a = IxS $ \Empty -> ((), Ext (Var :-> a :! Eff) Empty)

-- Indexed monad instance
instance IxMonad IxState where
    type Inv IxState s t = (Split (Reads s) (Reads t) (Reads (UnionS s t)), 
                            Unionable (Writes s) (Writes t), 
                            Intersectable (Writes s) (Reads t), 
                            Writes (UnionS s t) ~ UnionS (Writes s) (Writes t))
    type Unit IxState = '[]
    type Plus IxState s t = UnionS s t

    return x = IxS $ \Empty -> (x, Empty)

    (IxS e) >>= k = 
        IxS $ \i -> let (sR, tR) = split i
                        (a, sW)  = e sR
                        (b, tW) = (runState $ k a) (sW `intersectReads` tR)
                    in  (b, sW `union` tW) 

{-
instance Subeffect IxState where
    type Join IxState s t = Union s t
    type SubInv IxState s t = Split s t (Union s t)
    subEffect p (IxR e) = IxR $ \st -> let (s, t) = split st 
                                           _ = ReflP p t 
                                       in e s

-- Equality proof between a set and a proxy
data EqT a b where
    ReflP :: Proxy t -> Set t -> EqT t -}