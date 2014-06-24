{-# LANGUAGE RebindableSyntax, NoMonomorphismRestriction, DataKinds, TypeOperators, 
   FlexibleContexts, ConstraintKinds #-}
import Prelude hiding (Monad(..))
import Control.IxMonad
import Control.IxMonad.Reader
import Control.IxMonad.Helpers.Set

import GHC.TypeLits
import Data.Proxy

{- Examples -}

-- foo :: Reader '["x" :-> a, "xs" :-> [a]] [a]
foo = do x <- ask (Var::(Var "x"))
         xs <- ask (Var::(Var "xs"))
         x' <- ask (Var::(Var "x"))
         return (x:x':xs)

init1 = Ext (Var :-> 1) (Ext (Var :-> [2, 3]) Empty)
runFoo = runReader foo init1

-- Examples with subeffecting (need to refine the types a bit to 'run')

bar :: (Subset '["x" :-> Int, "xs" :-> [Int]] t) => Reader t [Int]
bar = sub foo

init2 :: Set '["x" :-> Int, "xs" :-> [Int], "z" :-> a]
init2 =  Ext (Var :-> 1) (Ext (Var :-> [2, 3]) (Ext (Var :-> undefined) Empty))
runBar = runReader bar init2

-- Note: GHC currently has trouble with
-- init2 = asSet (append init1 (Ext (Var :-> undefined) Empty))


