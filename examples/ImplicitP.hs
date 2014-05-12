{-# LANGUAGE ImplicitParams, DataKinds, RebindableSyntax, TypeOperators #-}

import Prelude hiding (Monad(..))
import Control.IxMonad
import Control.IxMonad.Reader

foo :: (Num a, ?x :: a, ?y :: a) => a -> a
foo z = ?x + ?y

fooM :: (Num a) => a -> IxReader '["?x" :-> a, "?y" :-> a] a
fooM z = do x <- ask (Var::(Var "?x"))
            y <- ask (Var::(Var "?y"))
            return (x + y)

foo' :: (Num a, ?y :: a) => a 
foo' = let ?x = 42
       in  ?x + ?y

foo'' :: (Num a, ?x :: a, ?y :: a, ?z :: a) => a
foo'' = ?x + ?y