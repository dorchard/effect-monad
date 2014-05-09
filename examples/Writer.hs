{-# LANGUAGE DataKinds, KindSignatures, TypeOperators, RebindableSyntax, FlexibleInstances #-}

import Control.IxMonad
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
