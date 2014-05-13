{-# LANGUAGE RebindableSyntax, TypeOperators, DataKinds, KindSignatures, FlexibleInstances, 
              ConstraintKinds, FlexibleContexts, TypeFamilies #-}

import Prelude hiding (Monad(..))
import Control.IxMonad
import Control.IxMonad.Helpers.Set
import Control.IxMonad.State

parMap :: (IsSet f, StateSet f, Writes f ~ '[]) => (a -> State f b) -> [a] -> State f [b] 
-- parMap k [] = sub (return [])
parMap k [x] = do y <- k x
                  return [y]
parMap k (x:xs) = do y  <- k x
                     ys <- parMap k xs
                     return (y : ys)

{-

parMap2 :: (StateSet f, Writes f ~ '[]) => (a -> State f b) -> [a] -> State f [b] 
parMap2 k [] = sub (return [])
parMap2 k (x:xs) = do y  <- k x
                      ys <- parMap2 k xs
                      return (y : ys)-}