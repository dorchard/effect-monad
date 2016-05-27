{-# LANGUAGE RebindableSyntax, NoMonomorphismRestriction #-}

import Prelude hiding (Monad(..))
import Control.Effect
import Control.Effect.Cond
import Control.Effect.ReadOnceReader

foo = do x <- ask
         y <- ask
         return ("Name " ++ x ++ " age " ++ (show y))

--foo_eval = foo (HCons' 'a' (HCons' "bc" HNil'))

foo2 = do x <- ask
          y <- ask
          xs <- ask
          return (x : (y : xs))

foo2' = do x <- ask
           xs' <- do y <- ask
                     xs <- ask
                     return (y:xs)
           return (x : xs')

foo2_eval foo2 = runReader foo2 (Cons 'a' (Cons 'b' (Cons "c" Nil)))

foo3 = do x <- ask
          ifM x ask (return 0)

foo3_eval = runReader foo3 (Cons False (Cons 42 Nil))