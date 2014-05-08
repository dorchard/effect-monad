{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, RebindableSyntax #-}

module Control.IxMonad.Update where 

import Control.IxMonad
import Data.HList hiding (Monad(..), append)
import Prelude hiding (Monad(..))

data Put a = Put a deriving Show
data NoPut = NoPut deriving Show

-- Uupdate monad
instance IxMonad (,) where -- i.e., m p a = (p, a)
    type Inv (,) s t = UpdateBind s t

    type Unit (,) = NoPut
    type Plus (,) s NoPut = s
    type Plus (,) s (Put t) = Put t

    return x = (NoPut, x)
    x >>= k = bind x k 

class UpdateBind s t where
    bind :: (s, a) -> (a -> (t, b)) -> (Plus (,) s t, b)

instance UpdateBind s NoPut where
    bind (s, a) k = let (NoPut, b) = k a in (s, b)

instance UpdateBind s (Put t) where
    bind (s, a) k = k a

put :: a -> (Put a, ())
put x = (Put x, ())


{- In GHC 7.8 can get rid of the 'Put' wrapper and use 'closed' family

type Plus (,) s t where
  Plus (,) s () = s
  Plus (,) s t  = t   
 
-}
