{-# LANGUAGE DataKinds, RebindableSyntax, TypeOperators, FlexibleInstances #-}

import Prelude hiding (Monad(..))
import Control.IxMonad
import Control.IxMonad.State

x_var = Var::(Var "x")
y_var = Var::(Var "y")

foo :: IxState '["x" :-> Int :! R, "y" :-> [Int] :! RW] [Int]
foo = do x <- get x_var
         y <- get y_var
         put y_var (x:y)
         z <- get y_var
         return (x:z)

foo_run = runState foo (Ext (x_var :-> (1 :! Eff)) (Ext (y_var :-> ([2,3] :! Eff)) Empty))

instance Show (Var "x") where
    show _ = "x"

instance Show (Var "y") where
    show _ = "y"


foo2 :: IxState '["x" :-> Int :! RW] Int
foo2 = do x <- get x_var
          put x_var (x+1)
          return x

foo2_run = (runState foo2) (Ext (x_var :-> 10 :! Eff) Empty)

