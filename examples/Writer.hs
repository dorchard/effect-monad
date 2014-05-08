{-# LANGUAGE DataKinds, KindSignatures, TypeOperators, RebindableSyntax #-}

import Control.IxMonad
import Control.IxMonad.Writer

import Data.Monoid

import Prelude hiding (Monad(..))

instance Monoid Int where
    mappend = (+)
    mempty  = 0

test :: Writer '["x" :-> Int, "y" :-> String, "z" :-> Bool] ()
test = do -- ...
          put (Proxy::(Proxy "x")) (42::Int)
          -- ...
          put (Proxy::(Proxy "y")) "hello"
          -- ....
          put (Proxy::(Proxy "z")) True 
          -- .....
          put (Proxy::(Proxy "x")) (58::Int) -- update to 'x'

