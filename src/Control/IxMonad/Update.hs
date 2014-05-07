{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, RebindableSyntax, GADTs #-}

module Control.IxMonad.Update where 

import Control.IxMonad
import Prelude hiding (Monad(..))

data Put a
data NoPut

data Update p where
   Put :: a -> Update (Put a)
   NoPut :: Update NoPut

data IxUpdate w a = IxUpdate { runIxUpdate :: (a, Update w) }

-- Uupdate monad
instance IxMonad IxUpdate where 
    type Unit IxUpdate = NoPut
    type Plus IxUpdate s NoPut   = s
    type Plus IxUpdate s (Put t) = Put t

    return x = IxUpdate (x, NoPut)
    (IxUpdate (a, w)) >>= k = IxUpdate (update w (runIxUpdate (k a)))
                               
update :: Update s -> (b, Update t) -> (b, Update (Plus IxUpdate s t))
update w (b, NoPut) = (b, w)
update _ (b, Put w) = (b, Put w)

put :: a -> IxUpdate (Put a) ()
put x = IxUpdate ((), Put x)