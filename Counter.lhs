> {-# LANGUAGE TypeFamilies, EmptyDataDecls #-}

> {-# LANGUAGE RebindableSyntax #-}

> import Control.IxMonad
> import Prelude hiding (Monad(..))

> data Z
> data S n

> data Counter n a = Counter { forget :: a }

> instance IxMonad Counter where
>     type Unit Counter = Z
>     type Plus Counter n Z = n
>     type Plus Counter n (S m) = S (Plus Counter n m)

>     return a = Counter a
>     (Counter a) >>= k = Counter . forget $ k a


> foo :: Counter Z Int
> foo = do x <- return 2
>          y <- return 4
>          return (x + y)

