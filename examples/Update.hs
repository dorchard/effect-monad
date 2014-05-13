{-# LANGUAGE RebindableSyntax, DataKinds #-}

import Prelude hiding (Monad(..))
import Control.Effect
import Control.Effect.Update

foo :: Update (Just String) ()
foo = do put 42
         put "hello"
         return ()