{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FlexibleContexts #-}

-- Bye Monads... as we know them
import Prelude hiding (Monad(..))
import Control.Effect.Parameterised.SafeFiles
import qualified System.IO as IO

example :: IO ()
example = runSafeFiles $ do
  h  <- openFile "foo" IO.ReadWriteMode
  h' <- openFile "bar" IO.ReadWriteMode
  x <- hGetChar h
  hPutChar h' x
  hClose h'
  hClose h
  return ()

example2 :: IO ()
example2 = runSafeFiles $ do
  h1  <- openFile "foo" IO.ReadWriteMode
  h2 <- openFile "bar" IO.ReadWriteMode
  loopy h1 h2

loopy h1 h2 = do
  isEmpty <- hIsEOF h1
  if isEmpty
    then do
      hClose h1
      hClose h2
    else do
      x <- hGetChar h1
      hPutChar h2 x
      loopy h1 h2
