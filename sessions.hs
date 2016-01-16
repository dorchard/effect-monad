{-# LANGUAGE RebindableSyntax, EmptyDataDecls, TypeOperators, DataKinds, KindSignatures, PolyKinds, TypeFamilies, ConstraintKinds, UndecidableInstances, NoMonomorphismRestriction, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, OverlappingInstances, GADTs, InstanceSigs #-}

import Prelude hiding (Monad(..))

import qualified Control.Concurrent.Chan as C
import qualified Control.Concurrent as Conc
import Control.Effect.Monad
import Control.Effect
import Control.Effect.Helpers.List
import GHC.TypeLits
import GHC.Exts
import Unsafe.Coerce
import Debug.Trace

-- IO monad operations wrapped with trivial effect system
newChan = Wrap $ C.newChan
writeChan x y = Wrap $ C.writeChan x y
readChan x = Wrap $ C.readChan x
type Chan = C.Chan
forkIO = Wrap . Conc.forkIO . unWrap
killThread = Wrap . Conc.killThread 

-- Sequential and parallel processes 
data s :| t
data P (s :: [*]) 

data Session (s :: *) a where
   Session :: (List s -> IO a) -> Session (P s) a
   Branch :: (t ~ Dual s) => Session (P s) a -> Session (P t) b -> Session (s :| t) (a, b)

instance Effect Session where
    type Plus Session (P s) (P t) = P (s :++ t)
    type Unit Session     = P '[]
    type Inv Session s t = (Binder s t)

    return x = Session $ \Nil -> unWrap $ return x
    (>>=) = binder

class Binder s t where
    binder :: (Session s a) -> (a -> Session t b) -> Session (Plus Session s t) b

instance Split s t => Binder (P s) (P t) where
    binder (Session x) k = 
        Session $ \xs -> let (s, t) = (split xs) :: (List s, List t)
                         in unWrap $ do a <- Wrap (x s)
                                        Wrap $ case (k a) of Session y -> y t

-- * Core combinators 
data n :? a = R (Channel n) (Chan a)
data n :! a = S (Channel n) (Chan a)

data Channel (n :: Symbol) = Channel

send :: Channel n -> t -> Session (P '[n :! t]) ()
send c x = Session $ \(Cons (S _ ch) Nil) -> unWrap $ (writeChan ch x) >> return ()

recv :: Channel n -> Session (P '[n :? t]) t
recv c = Session $ \(Cons (R _ ch) Nil) -> unWrap $ readChan ch

par :: Session (P s) a -> Session (P (Dual s)) b -> Session (s :| (Dual s)) (a, b)
par (Session s) (Session t) = Branch (Session s) (Session t)

type family Dual s where
    Dual '[] = '[]
    Dual ((s :? a) ': xs) = (s :! a) ': (Dual xs)
    Dual ((s :! a) ': xs) = (s :? a) ': (Dual xs)

-- * Examples

alice = Channel :: Channel "Alice"
bob   = Channel :: Channel "Bob"

-- satisfiable with a single thread
examplea = do send alice 42
              x <- recv alice
              return $ x + 1

example1 = do send alice 42
              x <- recv bob
              return x

example2 = do x <- recv alice
              send bob "hi"
              return x

example3 = example1 `par` example2

run_example = evalSession example3


example4 q = \p -> do x <- recv p
                      send q x
                      return x

-- * Various functions relating to the actual evaluation of sessions

class EvalSession s where
    evalSession :: Session s a -> IO a

instance (MkChans (Chans s), ExpandChans s (Chans s) s) => EvalSession (P s) where
    evalSession s@(Session k) = unWrap $ do chans <- Wrap $ (mkChans :: (IO (List (Chans s))))
                                            chans' <- return $ expandChans s chans
                                            x <- Wrap $ k chans'
                                            return x

instance (MkChans (Chans s), ExpandChans s (Chans s) s, ExpandChans t (Chans s) t) => EvalSession (s :| t) where
    evalSession (Branch (Session s) (Session t)) = 
             unWrap $ do chans <- Wrap $ (mkChans :: (IO (List (Chans s)))) -- since chanels must be dual, by construction
                         chansA <- return $ expandChans (Session s) chans
                         chansB <- return $ expandChans (Session t) chans
                         resultA <- newChan
                         resultB <- newChan
                         ta <- forkIO $ (Wrap $ s chansA) >>= (\x' -> writeChan resultA x')
                         tb <- forkIO $ (Wrap $ t chansB) >>= (\y' -> writeChan resultB y')
                         x <- readChan resultA
                         y <- readChan resultB
                         killThread ta
                         killThread tb
                         return (x, y)

type family LastName c (s :: [*]) where
    LastName c '[] = '[c]
    LastName c (c ': xs) = xs
    LastName c (d ': xs) = d ': (LastName c xs) 

type family Names (xs :: [*]) (ys :: [*]) where
    Names '[] ys           = ys
    Names xs  '[]          = xs
    Names (c ': xs) (y ': ys) = Names xs (LastName c ys)

type Chans xs = Names (RemActions xs) (RemActions xs)

type family RemActions (s :: [*]) where
            RemActions '[] = '[]
            RemActions ((c :? t) ': xs) = (Channel c, Chan ()) ': (RemActions xs)
            RemActions ((c :! t) ': xs) = (Channel c, Chan ()) ': (RemActions xs)


class MkChans s where
    mkChans :: IO (List s)

instance MkChans '[] where
    mkChans = unWrap $ return Nil

instance MkChans xs => MkChans ((Channel p, Chan ()) ': xs) where
    mkChans = unWrap $ do xs <- Wrap $ ((mkChans) :: IO (List xs))
                          x <- newChan 
                          return (Cons (Channel :: (Channel p), x) xs)

class ExpandChans s k t where
    expandChans :: Session (P s) a -> List k -> List t

instance ExpandChans '[] k '[] where
    expandChans (Session s) x = Nil

instance (ExpandChans xs k ys, LookUpA p k) => ExpandChans ((p :? t) ': xs) k ((p :? t) ': ys) where
    expandChans (Session s) xs = 
        Cons (R (Channel :: (Channel p)) ((unsafeCoerce ((lookupA xs (undefined :: Channel p))::(Chan ()))) :: (Chan t)))
               ((expandChans ((unsafeCoerce (Session s))::(Session (P xs) a)) xs) :: (List ys))

instance (ExpandChans xs k ys, LookUpA p k) => ExpandChans ((p :! t) ': xs) k ((p :! t) ': ys) where
    expandChans (Session s) xs = 
        Cons (S (Channel :: (Channel p)) ((unsafeCoerce ((lookupA xs (undefined :: Channel p))::(Chan ()))) :: (Chan t)))
               ((expandChans ((unsafeCoerce (Session s))::(Session (P xs) a)) xs) :: (List ys))

class LookUpA k xs where
    lookupA :: List xs -> Channel k -> Chan ()

instance LookUpA k ((Channel k, Chan ()) ': xs) where
    lookupA (Cons (_, x) _) k = x 
    
instance LookUpA k xs => LookUpA k ((j, w) ': xs) where
    lookupA (Cons _ xs) k = lookupA xs k

