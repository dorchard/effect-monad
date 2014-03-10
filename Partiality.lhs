> {-# LANGUAGE EmptyDataDecls #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE GADTs #-}

> import IxMonad
> import IxCondM

> data F 
> data T 
> data U


indexed partiality

> data IMaybe p a where
>     Fail :: IMaybe F a
>     Ok :: a -> IMaybe T a
>     U :: IMaybe s a -> IMaybe U a -- dynamic partiality

> instance Show a => Show (IMaybe p a) where
>     show Fail = "Fail"
>     show (U a) = show a
>     show (Ok a) = show a

> instance IxMonad IMaybe where
>   type Unit IMaybe = T

>   type Plus IMaybe F s = F -- conjunction
>   type Plus IMaybe T s = s
>   type Plus IMaybe U s = U

   type Plus IMaybe F F = F  -- conjunction
   type Plus IMaybe F T = F  --
   type Plus IMaybe T F = F  --
   type Plus IMaybe T T = T  --

>   ireturn x = Ok x

>   -- static
>   ibind k (Ok x) = k x
>   ibind k Fail = Fail

>   -- dynamic (statically undecidable)
>   ibind k (U (Ok a)) = U (k a)
>   ibind k (U Fail) = U Fail

> instance IxCondM IMaybe where
>     type Alt IMaybe T T = T
>     type Alt IMaybe F F = F

>     type Alt IMaybe F T = U
>     type Alt IMaybe T F = U

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
