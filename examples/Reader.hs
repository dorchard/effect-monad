{-# LANGUAGE RebindableSyntax, NoMonomorphismRestriction, DataKinds, TypeOperators #-}

import Prelude hiding (Monad(..))
import Control.IxMonad
import Control.IxMonad.Reader

import GHC.TypeLits
import Data.Proxy

{- Examples -}

-- foo :: IxReader '["x" :-> a, "xs" :-> [a]] [a]
foo = do x <- ask (Var::(Var "x"))
         xs <- ask (Var::(Var "xs"))
         return (x:xs)

runFoo = runReader foo (Ext (Var :-> 1) (Ext (Var :-> [2, 3]) Empty))

foo2 :: IxReader '["x" :-> a, "xs" :-> [a], "y" :-> b] [a]
foo2 = subEffect (Proxy::(Proxy '["y" :-> b])) foo 

runFoo2 = runReader foo2 (Ext (Var :-> 1) (Ext (Var :-> [2, 3]) (Ext (Var :-> undefined) Empty)))