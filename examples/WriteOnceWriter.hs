{-# LANGUAGE RebindableSyntax, NoMonomorphismRestriction #-}

import Prelude hiding (Monad(..))
import Control.Effect
import Control.Effect.WriteOnceWriter 

foo = do put 42
         put "hello"
         return ()