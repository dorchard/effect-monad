{-# LANGUAGE DataKinds, KindSignatures, TypeOperators, RebindableSyntax #-}

import Control.IxMonad
import Control.IxMonad.Writer

import Prelude hiding (Monad(..))

test :: Writer '["x" :-> Int, "y" :-> String, "z" :-> Bool] ()
test = do put (Proxy::(Proxy "x")) 42
          put (Proxy::(Proxy "y")) "hello"
          put (Proxy::(Proxy "z")) True 

