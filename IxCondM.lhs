> {-# LANGUAGE TypeFamilies #-}

> module IxCondM where

> class IxCondM m where
>     type Alt m s t 
>     ifM :: Bool -> m s a -> m t a -> m (Alt m s t) a
