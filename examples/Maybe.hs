{-# LANGUAGE RebindableSyntax, NoMonomorphismRestriction #-}

import Prelude hiding (Monad(..))
import Control.IxMonad
import Control.IxMonad.Cond
import Control.IxMonad.Maybe

headM x = ifM (x == []) (INothing) (IJust (head x))

foo x y = do x' <- headM x
             y' <- headM y
             return [x', y']