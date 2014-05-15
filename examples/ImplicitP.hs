{-# LANGUAGE ImplicitParams, DataKinds, RebindableSyntax, TypeOperators #-}

import Prelude hiding (Monad(..))
import Control.Effect
import Control.Effect.Reader

foo :: (Num a, ?x :: a, ?y :: a) => a -> a
foo z = ?x + ?y

fooM :: (Num a) => a -> Reader '["?x" :-> a, "?y" :-> a] a
fooM z = do x <- ask (Var::(Var "?x"))
            y <- ask (Var::(Var "?y"))
            return (x + y + z)

foo' :: (Num a, ?y :: a) => a -> a
foo' = let ?x = 42
       in \z -> ?x + ?y + z

fooM' :: Num a => Reader '["?x" :-> a] (a -> Reader '["?y" :-> a] a)
fooM' = merge (\z -> do x <- ask (Var::(Var "?x"))
                        y <- ask (Var::(Var "?y"))
                        return (x + y + z))

with f x = runReader f x

fooM'' = fooM' `with` (Ext (Var :-> 42) Empty)

sum2 :: Num a => a -> Reader '["?y" :-> a] a
sum2 = let x = (Ext ((Var::(Var "?x")) :-> 42) Empty) 
       in runReader (merge (\z -> do x <- ask (Var::(Var "?x"))
                                     y <- ask (Var::(Var "?y"))
                                     return (x + y + z))) x

     