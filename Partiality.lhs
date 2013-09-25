> {-# LANGUAGE EmptyDataDecls #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE GADTs #-}

> import IxMonad
> import IxCondM

> data F 
> data T 
> data U

indexed partiality

> data IExcp p a where
>     Fail :: IExcp F a
>     Ok :: a -> IExcp T a
>     U :: IExcp s a -> IExcp U a -- dynamic partiality

> instance Show a => Show (IExcp p a) where
>     show Fail = "Fail"
>     show (U a) = show a
>     show (Ok a) = show a

> instance IxMonad IExcp where
>   type Unit IExcp = T

>   type Plus IExcp F s = F -- conjunction
>   type Plus IExcp T s = s
>   type Plus IExcp U s = U

   type Plus IExcp F F = F  -- conjunction
   type Plus IExcp F T = F  --
   type Plus IExcp T F = F  --
   type Plus IExcp T T = T  --

>   ireturn x = Ok x

>   -- static
>   ibind k (Ok x) = k x
>   ibind k Fail = Fail

>   -- dynamic (statically undecidable)
>   ibind k (U (Ok a)) = U (k a)
>   ibind k (U Fail) = U Fail

> instance IxCondM IExcp where
>     type Alt IExcp T T = T
>     type Alt IExcp F F = F

>     type Alt IExcp F T = U
>     type Alt IExcp T F = U

>     -- statically decidable
>     ifM True (Ok x) (Ok y) = Ok x
>     ifM False (Ok x) (Ok y) = Ok y
>     ifM True Fail Fail = Fail
>     ifM False Fail Fail = Fail

>     -- dynamic (statically undecidable)
>     ifM True Fail (Ok x) = U Fail
>     ifM False Fail (Ok x) = U (Ok x)
>     ifM True (Ok x) Fail = U (Ok x)
>     ifM False (Ok x) Fail = U Fail

> headE x = ifM (x == []) (Fail) (Ok (head x))

> fooa x y = headE x >>=: (\x' -> headE y >>=: (\y' -> ireturn [x', y']))
