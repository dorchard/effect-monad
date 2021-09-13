{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Control.Effect.Parameterised.ExtensibleState
          (State(..), Get, Put, Update, get, put, modify
          , PMonad(..), (>>), ifThenElse, fail) where

import Prelude hiding (fail, Monad(..))
import Control.Effect.Parameterised
import Control.Effect.Parameterised.State
import Data.Type.Map

-- Fine-grained get, put, and modify
get :: IsMember v t m => Var v -> State (Map m) (Map m) t
get v = State $ \s -> (lookp v s, s)

put :: Updatable v t m n => Var v -> t -> State (Map m) (Map n) ()
put v t = State $ \s -> ((), update s v t)

modify :: (IsMember v s m, Updatable v t m n) => Var v -> (s -> t) -> State (Map m) (Map n) ()
modify v f = do
  x <- get v
  put v (f x)

-- Aliases for our operations
type Get v t m = IsMember v t m
type Put v t m n = Updatable v t m n
type Update v t m = (Get v t m, Put v t m m)
