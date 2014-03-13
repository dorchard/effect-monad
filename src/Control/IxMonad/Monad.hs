{-# LANGUAGE TypeFamilies, GADTs #-}

module Control.IxMonad.Monad where

{- 

All monads are indexed monads with a trivial singleton-monoid index. 

This wrapper wraps normal monads into the IxMonad interface using the "M"
contructor.

-}

import Control.IxMonad
import Prelude hiding (Monad(..))
import qualified Prelude as P

data Monad m t a where
    Wrap :: P.Monad m => m a -> Monad m () a

unWrap :: Monad m t a -> m a
unWrap (Wrap m) = m

instance (P.Monad m) => IxMonad (Monad m) where
    -- trivial singleton monoid
    type Unit (Monad m)       = ()
    type Plus (Monad m) s t   = ()

    return x = Wrap (P.return x)
    (Wrap x) >>= f = Wrap ((P.>>=) x (unWrap . f))
    

    