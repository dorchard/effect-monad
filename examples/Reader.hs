{-# LANGUAGE RebindableSyntax, NoMonomorphismRestriction, DataKinds, TypeOperators, 
   FlexibleContexts, ConstraintKinds #-}
import Prelude hiding (Monad(..))
import Control.Effect
import Control.Effect.Reader

import GHC.TypeLits

{- Examples -}

-- foo :: Reader '["x" :-> a, "xs" :-> [a]] [a]
foo = do x <- ask (Var::(Var "x"))
         xs <- ask (Var::(Var "xs"))
         x' <- ask (Var::(Var "x"))
         return (x:x':xs)

runFoo = runReader foo (Ext (Var :-> 1) (Ext (Var :-> [2, 3]) Empty))

-- Examples with subeffecting (need to refine the types a bit to 'run')

foo2 :: (Subset '["x" :-> Int, "xs" :-> [Int]] t) => Reader t [Int]
foo2 = sub foo

init0 :: Set '["x" :-> Int, "xs" :-> [Int], "z" :-> a]
init0 = Ext (Var :-> 1) (Ext (Var :-> [2, 3]) (Ext (Var :-> undefined) Empty))

runFoo2 = runReader foo2 init0


