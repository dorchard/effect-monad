{-# LANGUAGE RebindableSyntax, NoMonomorphismRestriction #-}

import Prelude hiding (Monad(..))
import Control.IxMonad
import Control.IxMonad.Cond
import Control.IxMonad.Reader

foo = do x <- ask
         y <- ask 
         return ("Name " ++ x ++ " age " ++ (show y))

--foo_eval = foo (HCons 'a' (HCons "bc" HNil))

foo2 = do x <- ask 
          y <- ask
          xs <- ask
          return (x : (y : xs))

foo2' = do x <- ask 
           xs' <- do y <- ask
                     xs <- ask
                     return (y:xs)
           return (x : xs')

foo2_eval foo2 = foo2 (HCons 'a' (HCons 'b' (HCons "c" HNil)))

foo3 = do x <- ask
          ifM x ask (return 0)

foo3_eval = foo3 (HCons False (HCons 42 HNil))