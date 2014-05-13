{-# LANGUAGE TypeFamilies, GADTs #-}

module Control.Effect.Monad where

{- 

All monads are indexed monads with a trivial singleton-monoid index. 

This wrapper wraps normal monads into the Effect interface using the "M"
contructor.

-}

import Control.Effect
import Prelude hiding (Monad(..))
import qualified Prelude as P

data Monad m t a where
    Wrap :: P.Monad m => m a -> Monad m () a

unWrap :: Monad m t a -> m a
unWrap (Wrap m) = m

instance (P.Monad m) => Effect (Monad m) where
    -- trivial singleton monoid
    type Inv (Monad m) s t    = ()
    type Unit (Monad m)       = ()
    type Plus (Monad m) s t   = ()

    return x = Wrap (P.return x)
    (Wrap x) >>= f = Wrap ((P.>>=) x (unWrap . f))
    

    