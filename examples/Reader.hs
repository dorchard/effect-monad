{-# LANGUAGE RebindableSyntax, NoMonomorphismRestriction, DataKinds, TypeOperators #-}

import Prelude hiding (Monad(..))
import Control.IxMonad
import Control.IxMonad.Reader

import GHC.TypeLits
import Data.Proxy

{- Examples -}

foo :: IxReader '["x" :-> a, "xs" :-> [a]] [a]
foo = do x <- ask (Var::(Var "x"))
         xs <- ask (Var::(Var "xs"))
         return (x:xs)

runFoo = runReader foo (Ext (Var :-> 1) (Ext (Var :-> [2, 3]) Empty))
