{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, RebindableSyntax, 
             GADTs, EmptyDataDecls #-}

module Control.IxMonad.Update where 

import Control.IxMonad
import Prelude hiding (Monad(..))

data Put a
data NoPut

data Action p where
   Put :: a -> Action (Put a)
   NoPut :: Action NoPut

data Update w a = Update { runUpdate :: (a, Action w) }

-- Uupdate monad
instance IxMonad Update where 
    type Unit Update = NoPut
    type Plus Update s NoPut   = s
    type Plus Update s (Put t) = Put t

    return x = Update (x, NoPut)
    (Update (a, w)) >>= k =
         Update $ update w (runUpdate $ k a)
                               
update :: Action s -> (b, Action t) -> (b, Action (Plus Update s t))
update w (b, NoPut)   = (b, w)
update _ (b, Put w'') = (b, Put w'')

put :: a -> Update (Put a) ()
put x = Update ((), Put x)