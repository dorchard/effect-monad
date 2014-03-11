{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, RebindableSyntax #-}

import IxMonad
import Data.HList hiding (Monad(..), append)
import Prelude hiding (Monad(..))

data Put a = Put a deriving Show

instance IxMonad (,) where
    type Unit (,) = ()

    type Plus (,) s () = s
    type Plus (,) s (Put t) = Put t

    type Inv (,) s t = WOBind s t

    return x = ((), x)
    x >>= k = bind x k 

class WOBind s t where
    bind :: (s, a) -> (a -> (t, b)) -> (Plus (,) s t, b)

instance WOBind s () where
    bind (s, a) k = let ((), b) = k a in (s, b)

instance WOBind s (Put t) where
    bind (s, a) k = k a

put :: a -> (Put a, ())
put x = (Put x, ())

foo = do put 42
         put "hello"
         return ()



{- In GHC 7.8 can get rid of the 'Put' wrapper and use 'closed' family

type Plus (,) s t where
  Plus (,) s () = s
  Plus (,) s t  = t   
 
-}
