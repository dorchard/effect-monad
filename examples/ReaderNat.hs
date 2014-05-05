{-# LANGUAGE RebindableSyntax, NoMonomorphismRestriction #-}

import Prelude hiding (Monad(..))
import Control.IxMonad
import Control.IxMonad.Cond
import Control.IxMonad.ReaderNat

{- Examples -}

foo :: IxReader (Cons (Nat Z, a) Nil) a
foo = do x <- ask Z
         return x
      
-- unIxR foo (Cons (Z, 42) Nil)

foo2 :: IxReader (Cons (Nat Z, a) (Cons (Nat (S Z), [a]) Nil)) [a]
foo2 = do x <- ask Z
          xs <- ask (S Z)
          return (x : xs) 

-- unIxR foo2 (Cons (Z, 1) (Cons (S Z, [2,3]) Nil))
        
foo3 :: IxReader (Cons (Nat Z, a) (Cons (Nat (S Z), [a]) Nil)) [a]
foo3 = do xs <- ask (S Z)
          x <- ask Z
          return (x : xs) 
      
-- unIxR foo3 (Cons (Z, 1) (Cons (S Z, [2,3]) Nil))

--foo4 :: Num a => IxReader (Cons (Nat Z, a) Nil) a
foo4 = do x <- ask Z
          y <- ask Z
          return (x + y)

-- unIxR foo4 (Cons (Z, 42) Nil)
     
--foo5 :: IxReader (Cons (Nat Z, a) (Cons (Nat (S Z), [a]) Nil)) [a]
foo5 = do xs <- ask (S Z)
          x <- ask Z
          y <- ask Z
          return (x : (y : xs))

-- unIxR foo5 (Cons (Z, 1) (Cons (S Z, [2,3]) Nil))

--foo6 :: IxReader (Cons (Nat Z, a) (Cons (Nat (S Z), [a]) Nil)) [a]
foo6 = do x <- ask Z
          xs <- ask (S Z)
          y <- ask Z
          return (x : (y : xs))

-- unIxR foo6 (Cons (Z, 1) (Cons (S Z, [2,3]) Nil))