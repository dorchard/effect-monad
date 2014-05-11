{-# LANGUAGE DataKinds, KindSignatures, TypeOperators, RebindableSyntax, FlexibleInstances, 
             ConstraintKinds, FlexibleContexts, TypeFamilies 
  #-}

import Control.IxMonad
import Control.IxMonad.Helpers.Set
import Control.IxMonad.Writer

import Data.Monoid

import Prelude hiding (Monad(..))

instance Monoid Int where
    mappend = (+)
    mempty  = 0

test :: Writer '["x" :-> Int, "y" :-> String] ()
test = do -- ...
          put (Var::(Var "x")) (42::Int)
          -- ...
          put (Var::(Var "y")) "hello"
          -- .....
          put (Var::(Var "x")) (58::Int) -- update to 'x'

instance Show (Var "x") where
    show _ = "x"

instance Show (Var "y") where
    show _ = "y"

foo :: (IsSet f, Unionable f '["x" :-> Int], Num a) => 
       (a -> Writer f t) -> Writer (Union f '["x" :-> Int]) ()
foo f = do y <- f 3
           put (Var::(Var "x")) (42::Int)

foo2 :: (IsSet f, Unionable f '["x" :-> Int, "y" :-> t], Num a) => 
       (a -> Writer f t) -> Writer (Union f '["x" :-> Int, "y" :-> t]) ()
foo2 f = do y <- f 3
            put (Var::(Var "x")) (42::Int)
            put (Var::(Var "y")) y