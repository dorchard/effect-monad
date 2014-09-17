{-# LANGUAGE DataKinds, RebindableSyntax, TypeOperators, FlexibleInstances, NoMonomorphismRestriction #-}

import Prelude hiding (Monad(..))
import Control.Effect
import Control.Effect.State

x_var = Var::(Var "x")
y_var = Var::(Var "y")


{- Computation with a read effect on variable "x" and a read-write (update) effect on variable "y" -}
foo :: State '["x" :-> Int :! R, "y" :-> [Int] :! RW] [Int]
foo = do x <- get x_var
         y <- get y_var
         put y_var (x:y)
         z <- get y_var
         return (x:z)

foo_run = runState foo (Ext (x_var :-> (1 :! Eff)) (Ext (y_var :-> ([2,3] :! Eff)) Empty))


foo2 :: State '["x" :-> Int :! RW] Int
foo2 = do x <- get x_var
          put x_var (x+1)
          return x


foo2_run = (runState foo2) (Ext (x_var :-> 10 :! Eff) Empty)

