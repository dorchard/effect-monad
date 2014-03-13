{-# LANGUAGE TypeFamilies, RebindableSyntax #-}

import Control.IxMonad
import Control.IxMonad.Monad
import Prelude hiding (Monad(..)) 

putStrLn' = Wrap . putStrLn

printer = do putStrLn "Hello"
             putStrLn "I am really regular IO monad"
             putStrLn "hiding as the free indexed monad version"
             return ()