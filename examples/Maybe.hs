{-# LANGUAGE RebindableSyntax, NoMonomorphismRestriction #-}

import Prelude hiding (Monad(..))
import Control.Effect
import Control.Effect.Cond
import Control.Effect.Maybe

headM x = ifM (x == []) (INothing) (IJust (head x))

foo x y = do x' <- headM x
             y' <- headM y
             return [x', y']