{-# LANGUAGE DataKinds, RebindableSyntax, TypeOperators, FlexibleInstances #-}

import Prelude hiding (Monad(..))
import Control.Effect
import Control.Effect.State

x_var = Var::(Var "x")
y_var = Var::(Var "y")

{- Computation with a read effect on variable "x" and a
   read-write (update) effect on variable "y" -}
-- example :: State '["x" :-> Int :! R, "y" :-> [Int] :! RW] [Int]
example = do
  x <- get x_var
  y <- get y_var
  put y_var (x:y)
  z <- get y_var
  return (x:z)

initS =  Ext (x_var :-> (1 :! Eff)) (Ext (y_var :-> ([2,3] :! Eff)) Empty)
example_run = runState example initS

--example2 :: State '["x" :-> Int :! RW] Int
example2 = do
  x <- get (Var::(Var "x"))
  put (Var::(Var "x")) (x+1)
  return x

example2_run = (runState example2) (Ext (x_var :-> 10 :! Eff) Empty)

example3 :: State '["x" :-> String :! RW] ()
example3 = do
  x <- get x_var
  put x_var (x ++ " ok !")
