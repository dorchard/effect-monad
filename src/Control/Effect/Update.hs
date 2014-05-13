{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, RebindableSyntax, 
             GADTs, EmptyDataDecls, DataKinds #-}

module Control.Effect.Update where 

import Control.Effect
import Prelude hiding (Monad(..))


data Eff (w :: Maybe *) where
   Put :: a -> Eff (Just a)
   NoPut :: Eff Nothing

data Update w a = Update { runUpdate :: (a, Eff w) }

-- Uupdate monad
instance Effect Update where 
    type Inv Update s t = ()
    type Unit Update = Nothing
    type Plus Update s Nothing  = s
    type Plus Update s (Just t) = Just t

    return x = Update (x, NoPut)
    (Update (a, w)) >>= k =
         Update $ update w (runUpdate $ k a)
                               
update :: Eff s -> (b, Eff t) -> (b, Eff (Plus Update s t))
update w (b, NoPut)   = (b, w)
update _ (b, Put w'') = (b, Put w'')

put :: a -> Update (Just a) ()
put x = Update ((), Put x)