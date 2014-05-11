{-# LANGUAGE RebindableSyntax, NoMonomorphismRestriction, DataKinds, TypeOperators, 
   -- Remove this after experimenting with 'sub'
  MultiParamTypeClasses, FlexibleInstances, GADTs, FlexibleContexts, UndecidableInstances, OverlappingInstances #-}

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

-- Examples with subeffecting (need to refine the types a bit to 'run')

foo2 :: Sub '["x" :-> Int, "xs" :-> [Int]] t => IxReader t [Int]
foo2 = sub foo

runFoo2 = runReader foo2 (Ext ((Var::(Var "x")) :-> (1::Int)) (Ext ((Var::(Var "xs")) :-> ([2, 3]::[Int])) (Ext ((Var::(Var "z")) :-> undefined) Empty)))


