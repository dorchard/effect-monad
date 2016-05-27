{-# LANGUAGE ImplicitParams, DataKinds, RebindableSyntax, TypeOperators, ScopedTypeVariables #-}
import Prelude hiding (Monad(..))
import Control.Effect
import Control.Effect.Reader

import Data.Type.Map

import GHC.TypeLits

{- Examples -}

-- example :: Reader '["x" :-> a, "xs" :-> [a]] [a]
example = do
  x  <- ask (Var::(Var "x"))
  xs <- ask (Var::(Var "xs"))
  x' <- ask (Var::(Var "x"))
  return (x:x':xs)

init1 = Ext Var 1 (Ext Var [2, 3] Empty)
runExample = runReader example init1

-- Examples with subeffecting (need to refine the types a bit to 'run')

example' :: (Submap '["x" :-> Int, "xs" :-> [Int]] t) => Reader t [Int]
example' = sub example


init2 :: Map '["x" :-> Int, "xs" :-> [Int], "z" :-> a]
init2 =  Ext Var 1 (Ext Var [2, 3] (Ext Var undefined Empty))
runExample' = runReader example' init2
