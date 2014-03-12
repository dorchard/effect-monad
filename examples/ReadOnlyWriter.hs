{-# LANGUAGE RebindableSyntax, NoMonomorphismRestriction #-}

import Prelude hiding (Monad(..))
import Control.IxMonad
import Control.IxMonad.ReadOnlyWriter

foo = do put 42
         put "hello"
         return ()