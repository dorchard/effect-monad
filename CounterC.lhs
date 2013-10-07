%if False

> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE EmptyDataDecls #-}

> import IxComonad

%endif

> data Z
> data S n

> data Counter n a = Counter { forget :: a }

Adds "count" when composing using iextend

> instance IxComonad Counter where
>     type Unit Counter = Z
>     type Plus Counter n Z = n
>     type Plus Counter n (S m) = S (Plus Counter n m)

>     iextract x = forget x
>     iextend k (Counter a) = Counter (k (Counter a))