{-# LANGUAGE ImplicitParams, DataKinds, RebindableSyntax, TypeOperators #-}

import Prelude hiding (Monad(..))
import Control.Effect
import Control.Effect.Reader


example1 :: (Num a, ?x :: a, ?y :: a) => a -> a
example1 z = ?x + ?y

example1M :: (Num a) => a -> Reader '["?x" :-> a, "?y" :-> a] a
example1M z = do x <- ask (Var::(Var "?x"))
                 y <- ask (Var::(Var "?y"))
                 return (x + y + z)

example2 :: (Num a, ?y :: a) => a -> a
example2 = let ?x = 42
            in \z -> ?x + ?y + z

example2M :: Num a => Reader '["?x" :-> a] (a -> Reader '["?y" :-> a] a)
example2M = merge (\z -> do x <- ask (Var::(Var "?x"))
                            y <- ask (Var::(Var "?y"))
                            return (x + y + z))

with f x = runReader f x
runExample2 = example2M `with` (Ext Var (42 :: Int) Empty)
runExample2' x y z = (example2M `with` (Ext Var (x :: Int) Empty) $ z)
                                `with` (Ext Var y Empty)


example3M :: Int -> Reader '["?y" :-> Int] Int
example3M =
  let x = Ext (Var::(Var "?x")) (42 :: Int) Empty
  in runReader (merge (\z -> do x <- ask (Var::(Var "?x"))
                                y <- ask (Var::(Var "?y"))
                                return (x + y + z))) x
