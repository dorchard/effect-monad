{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances,
             UndecidableInstances, RebindableSyntax,  DataKinds,
             TypeOperators, PolyKinds, FlexibleContexts, ConstraintKinds,
             IncoherentInstances, GADTs
             #-}

module Control.Effect.State (Set(..), get, put, State(..), (:->)(..), (:!)(..),
                                  Eff(..), Action(..), Var(..), union, UnionS,
                                     Reads(..), Writes(..), Unionable, Sortable, SetLike,
                                      StateSet,
                                          --- may not want to export these
                                          IntersectR, Update, Sort, Split) where

import Control.Effect
import Data.Type.Set hiding (Unionable, union, SetLike, Nub, Nubable(..))
import qualified Data.Type.Set as Set
--import Data.Type.Map (Mapping(..), Var(..))

import Prelude hiding (Monad(..),reads)
import GHC.TypeLits

{-| Provides an effect-parameterised version of the state monad, which gives an
   effect system for stateful computations with annotations that are sets of
   variable-type-action triples. -}


{-| Distinguish reads, writes, and read-writes -}
data Eff = R | W | RW
{-| Provides a wrapper for effect actions -}
data Action (s :: Eff) = Eff

instance Show (Action R) where
    show _ = "R"
instance Show (Action W) where
    show _ = "W"
instance Show (Action RW) where
    show _ = "RW"

infixl 2 :->
data (k :: Symbol) :-> (v :: *) = (Var k) :-> v

data Var (k :: Symbol) where Var :: Var k
                             {- Some special defaults for some common names -}
                             X   :: Var "x"
                             Y   :: Var "y"
                             Z   :: Var "z"


instance (Show (Var k), Show v) => Show (k :-> v) where
    show (k :-> v) = "(" ++ show k ++ " :-> " ++ show v ++ ")"
instance Show (Var "x") where
    show X   = "x"
    show Var = "Var"
instance Show (Var "y") where
    show Y   = "y"
    show Var = "Var"
instance Show (Var "z") where
    show Z   = "z"
    show Var = "Var"
instance Show (Var v) where
    show _ = "Var"

{-| Symbol comparison -}
type instance Cmp (v :-> a) (u :-> b) = CmpSymbol v u



{-| Describes an effect action 's' on a value of type 'a' -}
--data EffMapping a (s :: Eff) = a :! (Action s)
data a :! (s :: Eff) = a :! (Action s)

instance (Show (Action f), Show a) => Show (a :! f) where
    show (a :! f) = show a ++ " ! " ++ show f

infixl 3 :!

type SetLike s = Nub (Sort s)
type UnionS s t = Nub (Sort (s :++ t))
type Unionable s t = (Sortable (s :++ t), Nubable (Sort (s :++ t)) (Nub (Sort (s :++ t))),
                      Split s t (Union s t))

{-| Union operation for state effects -}
union :: (Unionable s t) => Set s -> Set t -> Set (UnionS s t)
union s t = nub (quicksort (append s t))

{-| Type-level remove duplicates from a type-level list and turn different sorts into 'RW'| -}
type family Nub t where
    Nub '[]       = '[]
    Nub '[e]      = '[e]
    Nub (e ': e ': as) = Nub (e ': as)
    Nub ((k :-> a :! s) ': (k :-> a :! t) ': as) = Nub ((k :-> a :! RW) ': as)
    Nub (e ': f ': as) = e ': Nub (f ': as)

{-| Value-level remove duplicates from a type-level list and turn different sorts into 'RW'| -}
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


{-| Update reads, that is any writes are pushed into reads, a bit like intersection -}
class Update s t where
    update :: Set s -> Set t

instance Update xs '[] where
    update _ = Empty

instance Update '[e] '[e] where
    update s = s

{-
instance Update ((v :-> b :! R) ': as) as' => Update ((v :-> a :! s) ': (v :-> b :! s) ': as) as' where
    update (Ext _ (Ext (v :-> (b :! _)) xs)) = update (Ext (v :-> (b :! (Eff::(Action R)))) xs) -}

instance Update ((v :-> a :! R) ': as) as' => Update ((v :-> a :! W) ': (v :-> b :! R) ': as) as' where
    update (Ext (v :-> (a :! _)) (Ext _ xs)) = update (Ext (v :-> (a :! (Eff::(Action R)))) xs)

instance Update ((u :-> b :! s) ': as) as' => Update ((v :-> a :! W) ': (u :-> b :! s) ': as) as' where
    update (Ext _ (Ext e xs)) = update (Ext e xs)

instance Update ((u :-> b :! s) ': as) as' => Update ((v :-> a :! R) ': (u :-> b :! s) ': as) ((v :-> a :! R) ': as') where
    update (Ext e (Ext e' xs)) = Ext e $ update (Ext e' xs)

type IntersectR s t = (Sortable (s :++ t), Update (Sort (s :++ t)) t)

{-| Intersects a set of write effects and a set of read effects, updating any read effects with
    any corresponding write value -}
intersectR :: (Reads t ~ t, Writes s ~ s, IsSet s, IsSet t, IntersectR s t) => Set s -> Set t -> Set t
intersectR s t = update (quicksort (append s t))

{-| Parametric effect state monad -}
data State s a = State { runState :: Set (Reads s) -> (a, Set (Writes s)) }

{-| Calculate just the reader effects -}
type family Reads t where
    Reads '[]                    = '[]
    Reads ((k :-> a :! R) ': xs)  = (k :-> a :! R) ': (Reads xs)
    Reads ((k :-> a :! RW) ': xs) = (k :-> a :! R) ': (Reads xs)
    Reads ((k :-> a :! W) ': xs)  = Reads xs

{-| Calculate just the writer effects -}
type family Writes t where
    Writes '[]                     = '[]
    Writes ((k :-> a :! W) ': xs)  = (k :-> a :! W) ': (Writes xs)
    Writes ((k :-> a :! RW) ': xs) = (k :-> a :! W) ': (Writes xs)
    Writes ((k :-> a :! R) ': xs)  = Writes xs

{-| Read from a variable 'v' of type 'a'. Raise a read effect. -}
get :: Var v -> State '[v :-> a :! R] a
get _ = State $ \(Ext (v :-> (a :! _)) Empty) -> (a, Empty)

{-| Write to a variable 'v' with a value of type 'a'. Raises a write effect -}
put :: Var v -> a -> State '[v :-> a :! W] ()
put _ a = State $ \Empty -> ((), Ext (Var :-> a :! Eff) Empty)

{-| Captures what it means to be a set of state effects -}
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

    {-| Pure state effect is the empty state -}
    type Unit State = '[]
    {-| Combine state effects via specialised union (which combines R and W effects on the same
      variable into RW effects -}
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
