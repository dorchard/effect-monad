%if False

> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE EmptyDataDecls #-}

> import IxMonad

%endif

> data Z
> data S n

> data Counter n a = Counter { forget :: a }

> instance IxMonad Counter where
>     type Unit Counter = Z
>     type Plus Counter n Z = n
>     type Plus Counter n (S m) = S (Plus Counter n m)

>     ireturn a = Counter a
>     ibind k (Counter a) = Counter . forget $ k a
