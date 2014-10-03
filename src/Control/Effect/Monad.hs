{-# LANGUAGE TypeFamilies, GADTs #-}

module Control.Effect.Monad where

import Control.Effect
import Prelude hiding (Monad(..))
import qualified Prelude as P


{-| All monads are parametric effect monads with a trivial singleton-monoid index. 
This wrapper wraps normal monads into the Effect interface using the 'M' contructor.
-}

{-| Wrap regular monads up -}
data Monad m f a where
    Wrap :: P.Monad m => m a -> Monad m () a

{-| Unwrap a monad -}
unWrap :: P.Monad m => Monad m f a -> m a
unWrap (Wrap m) = m

instance (P.Monad m) => Effect (Monad m) where
    {-| Trivial singleton monoid -}
    type Inv (Monad m) s t    = ()
    type Unit (Monad m)       = ()
    type Plus (Monad m) s t   = ()

    return x = Wrap (P.return x)
    (Wrap x) >>= f = Wrap ((P.>>=) x (unWrap . f))
    

    