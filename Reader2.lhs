> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FlexibleInstances #-}

> import Data.HList

> import IxMonad
> import IxCondM

> data IReader2 p a = IR2 { iread2 :: p -> a }

> class AppendA s t where
>     type Append s t
>     appenda :: s -> t -> Append s t
>     split :: Append s t -> (s, t)

> instance AppendA HNil t where
>     type Append HNil t = t
>     appenda HNil t = t
>     split t = (HNil, t)

> instance AppendA xs ys => AppendA (HCons x xs) ys where
>     type Append (HCons x xs) ys = HCons x (Append xs ys)
>     appenda (HCons x xs) ys = HCons x (appenda xs ys)
>     split (HCons x xs) = let (xs', ys') = split xs
>                          in (HCons x xs', ys')



> instance IxMonad IReader2 where
>     type Inv IReader2 s t = AppendA s t

>     type Unit IReader2 = HNil
>     type Plus IReader2 s t = Append s t

>     ireturn x = IR2 $ \HNil -> x
>     ibind k (IR2 f) = IR2 $ \xs -> let (s, t) = split xs
>                                    in iread2 (k (f s)) t
 
> ask :: IReader2 (HCons a HNil) a
> ask = IR2 $ (\(HCons x HNil) -> x)






> foo' :: IReader2 (HCons a (HCons [a] HNil)) [a]
> foo' = ask >:>= (\x -> ask >:>= (\xs -> ireturn (x : xs)))
> foo'_eval = iread2 foo' (HCons 'a' (HCons "bc" HNil))

> foo2' = ask >:>= (\x -> ask >:>= (\y -> ask >:>= (\z -> ireturn (x, y, z))))

> foo2A, foo2B :: IReader2 (HCons a (HCons b (HCons c HNil))) (a, b, c)
> foo2A = ibind (\x -> ibind (\y -> ibind (\z -> ireturn (x, y, z)) ask) ask) ask
> foo2B = ibind (\(x, y) -> ibind (\z -> ireturn (x, y, z)) ask) $ 
>                ibind (\x -> ibind (\y -> ireturn (x, y)) ask) ask

> foo2Aeval = iread2 foo2A (HCons 1 (HCons 'a' (HCons True HNil)))
> foo2Beval = iread2 foo2B (HCons 1 (HCons 'a' (HCons True HNil)))
