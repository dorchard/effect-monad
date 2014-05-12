{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, GADTs, 
             EmptyDataDecls, UndecidableInstances, RebindableSyntax, OverlappingInstances, 
             DataKinds, TypeOperators, PolyKinds, NoMonomorphismRestriction, FlexibleContexts,
             AllowAmbiguousTypes, ScopedTypeVariables, FunctionalDependencies, ConstraintKinds, 
             InstanceSigs, IncoherentInstances #-}

module Control.IxMonad.State (Set(..), get, put, State(..), (:->)(..), (:!)(..),
                                  Eff(..), Effect(..), Var(..), union, UnionS, 
                                     Reads(..), Writes(..), Unionable, Sortable, SetLike, 
                                      StateSet, 
                                          --- may not want to export these
                                          Intersectable, UpdateReads, Sort, Split) where

import Control.IxMonad
import Control.IxMonad.Helpers.Mapping 
import Control.IxMonad.Helpers.Set hiding (Unionable, union, SetLike, Nub, Nubable(..))
import qualified Control.IxMonad.Helpers.Set as Set

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

type SetLike s = Nub (Sort s)
type UnionS s t = Nub (Sort (Append s t))
type Unionable s t = (Sortable (Append s t), Nubable (Sort (Append s t)) (Nub (Sort (Append s t))),
                      Split s t (Union s t))

union :: (Unionable s t) => Set s -> Set t -> Set (UnionS s t)
union s t = nub (bsort (append s t))

{-

{- Remove duplicates from a type-level list and turn different sorts into 'RW'
   extends the previous definition from Set module -}
type family Nub t where
            Nub ((k :-> a :! s) ': (k :-> a :! s) ': as) = Nub ((k :-> a :! s) ': as)
            Nub ((k :-> a :! s) ': (k :-> a :! t) ': as) = Nub ((k :-> a :! RW) ': as)
            Nub t = Set.Nub t

-}

type family Nub t where
    Nub '[]       = '[]
    Nub '[e]      = '[e]
    Nub ((k :-> a :! s) ': (k :-> b :! s) ': as) = Nub ((k :-> b :! s) ': as)
    Nub ((k :-> a :! s) ': (k :-> a :! t) ': as) = Nub ((k :-> a :! RW) ': as)
    Nub ((k :-> a :! s) ': (j :-> b :! t) ': as) = (k :-> a :! s) ': Nub ((j :-> b :! t) ': as)

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
    nub (Ext _ (Ext (k :-> (a :! _)) xs)) = nub (Ext (k :-> (a :! (Eff::(Effect RW)))) xs)

instance Nubable ((j :-> b :! t) ': as) as' => 
    Nubable ((k :-> a :! s) ': (j :-> b :! t) ': as) ((k :-> a :! s) ': as') where
    nub (Ext (k :-> (a :! s)) (Ext (j :-> (b :! t)) xs)) = Ext (k :-> (a :! s)) (nub (Ext (j :-> (b :! t)) xs))

{-
class Nubable t where
    nub :: Set t -> Set (Nub t)

instance Nubable ((k :-> a :! s) ': as) => 
           Nubable ((k :-> a :! s) ': (k :-> a :! s) ': as) where
    nub (Ext _ (Ext (k :-> (a :! _)) xs)) = nub (Ext (k :-> (a :! Eff)) xs)

instance Nubable ((k :-> a :! RW) ': as) => 
           Nubable ((k :-> a :! s) ': (k :-> a :! t) ': as) where
    nub (Ext _ (Ext (k :-> (a :! _)) xs)) = nub (Ext (k :-> (a :! (Eff::(Effect RW)))) xs)

instance (Nub t ~ Set.Nub t, Set.Nubable t) => Nubable t where
    nub x = Set.nub x
-}


{- Update reads, that is any writes are pushed into reads, a bit like intersection -}

class UpdateReads t v where
    updateReads :: Set t -> Set v

instance UpdateReads xs '[] where
    updateReads _ = Empty

instance UpdateReads '[e] '[e] where 
    updateReads s = s


instance UpdateReads ((k :-> b :! R) ': as) as' => UpdateReads ((k :-> a :! s) ': (k :-> b :! s) ': as) as' where
    updateReads (Ext _ (Ext (k :-> (b :! _)) xs)) = updateReads (Ext (k :-> (b :! (Eff::(Effect R)))) xs) 

instance UpdateReads ((k :-> a :! R) ': as) as' => UpdateReads ((k :-> a :! W) ': (k :-> b :! R) ': as) as' where
    updateReads (Ext (k :-> (a :! _)) (Ext _ xs)) = updateReads (Ext (k :-> (a :! (Eff::(Effect R)))) xs)

{-

instance UpdateReads ((k :-> a :! R) ': as) as' => UpdateReads ((k :-> a :! W) ': (k :-> b :! RW) ': as) as' where
    updateReads (Ext (k :-> (a :! _)) (Ext _ xs)) = updateReads (Ext (k :-> (a :! (Eff::(Effect R)))) xs)

instance UpdateReads ((k :-> a :! R) ': as) as' => UpdateReads ((k :-> a :! RW) ': (k :-> b :! W) ': as) as' where
    updateReads (Ext (k :-> (a :! _)) (Ext _ xs)) = updateReads (Ext (k :-> (a :! (Eff::(Effect R)))) xs)
-}


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

intersectReads :: (Reads t ~ t, 
                   IsSet s, IsSet t, Intersectable s t) => Set s -> Set t -> Set t
intersectReads s t = updateReads (bsort (append s t))

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

class Writesers t v where
    writes :: Set t -> Set v

instance Writesers '[] '[] where
    writes Empty = Empty

instance Writesers xs as' => Writesers ((k :-> a:! W) ': xs) ((k :-> a :! W) ': as') where
    writes (Ext x xs) = Ext x (writes xs)

instance Writesers xs as' => Writesers ((k :-> a:! RW) ': xs) ((k :-> a :! W) ': as') where
    writes (Ext (k :-> (v :! _)) xs) = Ext (k :-> (v :! Eff)) (writes xs)

instance Writesers xs as' => Writesers ((k :-> a:! R) ': xs) as' where
    writes (Ext x xs) = writes xs 




-- 'get/put' monadic primitives

get :: Var k -> State '[k :-> a :! R] a
get _ = State $ \(Ext (k :-> (a :! _)) Empty) -> (a, Empty)

put :: Var k -> a -> State '[k :-> a :! W] ()
put _ a = State $ \Empty -> ((), Ext (Var :-> a :! Eff) Empty)

{-
class Put t where
   put :: Var k -> a -> State '[k :-> a :! t] ()

instance Put W where
   put _ a = State $ \Empty -> ((), Ext (Var :-> a :! Eff) Empty)

instance Put RW where
   put _ a = State $ \Empty -> ((), Ext (Var :-> a :! Eff) Empty)-}

type StateSet f = (StateSetProperties f, StateSetProperties (Reads f), StateSetProperties (Writes f))
                   
type StateSetProperties f = (Intersectable f '[], Intersectable '[] f,
                             UnionS f '[] ~ f, Split f '[] f, 
                             UnionS '[] f ~ f, Split '[] f f, 
                             UnionS f f ~ f, Split f f f,
                             Unionable f '[], Unionable '[] f)
                   
-- Indexed monad instance
instance IxMonad State where
    type Inv State s t = (IsSet s, IsSet (Reads s), IsSet (Writes s),
                          IsSet t, IsSet (Reads t), IsSet (Writes t),
                          Reads (Reads t) ~ Reads t, -- Writes (Writes s) ~ Writes s, 
                            Split (Reads s) (Reads t) (Reads (UnionS s t)), 
                            Unionable (Writes s) (Writes t), 
                            Writesers s (Writes s), 
                            Intersectable (Writes s) (Reads t), 
                            Writes (UnionS s t) ~ UnionS (Writes s) (Writes t),
                         {-  Problem: 
                               UnionS '[x :-> T :! R] `[x :-> T :! W] = '[x :-> T :! RW]
                               Writes '[x :-> T :! RW] = '[x :-> T :! W]
                               
                               Writes '[x :-> T :! R] = '[]
                               Writes '[x :-> T :! W] = '[x :-> T :! W]
                               UnionS '[] '[x :-> T :! W] = '[x :-> T :! W]

                               UnionS '[x :-> T :! W] `[x :-> T :! W] = '[x :-> T :! RW]
                               Writes '[x :-> T :! RW] = '[x :-> T :! W]
                               
                               Writes '[x :-> T :! R] = '[]
                               Writes '[x :-> T :! W] = '[x :-> T :! W]
                               UnionS '[] '[x :-> T :! W] = '[x :-> T :! W]


                                    


-}
                         -- to remove
                            Show (Set (AsSet (Writes s)))
                         )
    type Unit State = '[]
    type Plus State s t = UnionS s t

    return x = State $ \Empty -> (x, Empty)

    (State e) >>= k = 
        State $ \i -> let (sR, tR) = split i
                          (a, sW)  = e sR
                          (b, tW) = ("trace: " ++ (show sW) ++ "\n") `trace` (runState $ k a) (sW `intersectReads` tR)
                      in  (b, sW `union` tW) 


{-

instance (Split s t (Union s t), Sub s t) => Subeffect State s t where
    sub (State e) = IxR $ \st -> let (s, t) = split st 
                                           _ = ReflP p t 
                                       in e s
-}


{-
instance Subeffect State where
    type Join State s t = Union s t
    type SubInv State s t = Split s t (Union s t)
    subEffect p (IxR e) = IxR $ \st -> let (s, t) = split st 
                                           _ = ReflP p t 
                                       in e s

-- Equality proof between a set and a proxy
data EqT a b where
    ReflP :: Proxy t -> Set t -> EqT t -}