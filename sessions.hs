{-# LANGUAGE RebindableSyntax, EmptyDataDecls, TypeOperators, DataKinds, KindSignatures, PolyKinds, TypeFamilies, ConstraintKinds, UndecidableInstances, NoMonomorphismRestriction #-}

import Prelude hiding (Monad(..))

import qualified Control.Concurrent.Chan as C
import Control.Effect.Monad
import Control.Effect
import Control.Effect.Helpers.List
import GHC.TypeLits
import GHC.Prim

newChan = Wrap $ C.newChan
writeChan x y = Wrap $ C.writeChan x y
readChan x = Wrap $ C.readChan x
type Chan = C.Chan


data Session (s :: [*]) a = Session (IO (List (Chans s), a))

-- List (Chans s) -> 

type family Chans (s :: [*]) where
    Chans '[] = '[]
    Chans ((s :? t) ': xs) = (Proc s, Chan t) ': (Chans xs)
    Chans ((s :! t) ': xs) = (Proc s, Chan t) ': (Chans xs)

type family MebChans (s :: [*]) where
    MebChans '[] = '[]
    MebChans ((s :? t) ': xs) = (Proc s, Chan t) ': (MebChans xs)
    MebChans ((s :! t) ': xs) = (Proc s, Chan t) ': (MebChans xs)

{-
type family LastT (s :: [*]) where
    LastT s t0 '[] ys = t0
    LastT s t0 ((s :? t) ': xs) ys = LastT s t xs ys
    LastT s t0 ((s :! t) ': xs) ys = LastT s t xs ys
    LastT s t0 ((s' :? t) ': xs) ys = LastT s t0 xs ((s' :! t) ': ys)
    LastT s t0 ((s' :! t) ': xs) ys = LastT s t0 xs ((s' :! t) ': ys) -}

type family EraseAction t where
    EraseAction (c :? t) = (Proc c, Chan t)
    EraseAction (c :! t) = (Proc c, Chan t)

type family Last c t (s :: [*]) where
    Last c t1 '[] = '[(c, t1)]
    Last c t1 ((c, t2) ': xs) = (c, t2) ': xs
    Last c t1 ((d, t2) ': xs) = (d, t2) ': (Last c t1 xs) -- non-matching proc names

type family LastsP (xs :: [*]) (ys :: [*]) where
    LastsP '[] ys           = ys
    LastsP ((c, t) ': xs) ((d, a) ': ys) = LastsP xs (Last c t ys)

{-

lastsP ::: [*] -> [*] -> [*]
lastsP '[] ys = ys
lastsP ((c, t) ': xs) ((d, a) ': ys) = lasts xs (lastr c t ys)
-}

type Lasts xs = LastsP xs xs

lastsm xs = lasts xs xs

lasts [] ys = ys
lasts ((c, t) : xs) ys = lasts xs (lastr c t (tail ys))

lastr c t [] = [(c, t)]
lastr c t1 ((d, t2) : xs) | c == d = (c, t2) : xs
                          | otherwise = (d, t2) : (lastr c t1 xs)

data s :? a
data s :! a 
type (s :: [*]) :| (t :: [*]) = s

instance Effect Session where
    type Plus Session s t = s :++ t
    type Unit Session     = '[]
    type Inv Session s t  = ()

    return x = Session $ unWrap $ return (Nil, x)
    x >>= k = undefined


data Proc (c :: Symbol) = Proc

-- * Core combinators

send :: Proc c -> t -> Session '[c :! t] ()
send c x = Session $ unWrap $ do ch <- newChan
                                 writeChan ch x
                                 return (Cons (c, ch) Nil, ())

recv :: Proc c -> Session '[c :? t] t
recv c = Session $ unWrap $ do ch <- newChan
                               x <- readChan ch 
                               return (Cons (c, ch) Nil, x)

par :: (t ~ Dual s) => Session s a -> Session t b -> Session (s :| t) (a, b)
par (Session x) (Session y) = undefined
    {-do  resultA <- newChan
        resultB <- newChan
        ta <- forkIO (x >>= (\x' -> writeChan resultA x'))
        tb <- forkIO (y >>= (\y' -> writeChan resultB x'))
        killThread ta
        killThread t b
        return (ta, tb)-}

type family Dual (s :: [*]) :: [*] where
    Dual '[] = '[]
    Dual ((s :? a) ': xs) = (s :! a) ': (Dual xs)
    Dual ((s :! a) ': xs) = (s :? a) ': (Dual xs)

-- * Examples

alice = Proc :: Proc "Alice"
bob   = Proc :: Proc "Bob"

example1 = do send alice 42
              x <- recv bob
              return x

example2 = do x <- recv alice
              send bob "hi"
              return x

example3 = example1 `par` example2             

