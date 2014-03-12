> {-# LANGUAGE TypeFamilies #-}

> import Control.IxMonad
> import Control.IxMonad.Cond
> import Prelude hiding (Monad(..))

> import Data.Char

> data IReader p a = IR { iread :: p -> a }

> instance IxMonad IReader where
>     type Unit IReader = ()
>     type Plus IReader s t = (s, t)
>     return x = IR $ \() -> x
>     (IR f) >>= k = IR $ \(s, t) -> (iread (k (f s)) t)

> ask :: IReader a a
> ask = IR $ id

> instance Cond IReader where
>     type Alt IReader s t = (s, t)
>     ifM True x y = IR $ \(p, _) -> (iread x) p 
>     ifM False x y = IR $ \(_, q) -> (iread y) q


Examples

> foo = ask >>= (\x -> ask >>= (\xs -> return (x : xs)))
> foo_eval = iread foo ('a', ("bc", ()))

> foo2 = ask >>= (\x -> ifM x (ask >>= (\y -> return $ y + 1))
>                              (ask >>= (\y -> return $ ord y)))

> foo2_eval1 = iread foo2 (True, ((42, ()), ('a', ())))
> foo2_eval2 = iread foo2 (False, ((42, ()), ('a', ())))
