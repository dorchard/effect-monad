{-# LANGUAGE RebindableSyntax, NoMonomorphismRestriction, DataKinds #-}

import Prelude hiding (Monad(..))
import Control.IxMonad
import Control.IxMonad.Reader

import GHC.TypeLits
import Data.Proxy

{- Examples -}

foo :: IxReader (Cons "x" a (Cons "xs" [a] Nil)) [a]
foo = do x <- ask (Proxy::(Proxy "x"))
         xs <- ask (Proxy::(Proxy "xs"))
         return (x:xs)

-- (unIxR foo) (Cons Proxy 1 (Cons Proxy [2,3] Nil))
