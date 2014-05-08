{-# LANGUAGE RebindableSyntax, NoMonomorphismRestriction #-}

import Prelude hiding (Monad(..))
import Control.IxMonad
import Control.IxMonad.Update

foo :: Update (Put String) ()
foo = do put 42
         put "hello"
         return ()