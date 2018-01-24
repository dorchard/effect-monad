{-# LANGUAGE RebindableSyntax #-}

-- Bye Monads... as we know them
import Prelude hiding (Monad(..))
-- Hello parameterised monads
import Control.Effect.Parameterised
import Control.Effect.Parameterised.AtomicState

-----------------------------
-- Examples

myProgram :: State (Closed Int) (Closed Int) String
myProgram = do
  x <- get
  put (x+1)
  a <- somethingPurish x
  return (a ++ show x)

somethingPurish :: Int -> State (Closed Int) (Closed Int) String
somethingPurish n = do
  x <- get
  put (x + 1)
  return $ if n == 0 then "hello" else "goodbye"
