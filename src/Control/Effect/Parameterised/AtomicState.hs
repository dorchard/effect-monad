-- For clarity in type classes instances
{-# LANGUAGE InstanceSigs #-}

module Control.Effect.Parameterised.AtomicState
        (get, put, Closed, Open, State(..)) where

-- Bye Monads... as we know them
import Prelude hiding (Monad(..))
-- Hello parameterised monads
import Control.Effect.Parameterised
import Control.Effect.Parameterised.State

newtype Closed s = Closed s deriving Show
newtype Open   s = Open s   deriving Show

-- get :: State s s
get :: State (Closed s) (Open s) s
get = State $ \(Closed s) -> (s, Open s)

-- put :: s -> State s ()
put :: t -> State (Open s) (Closed t) ()
put tx = State $ \(Open _) -> ((), Closed tx)
