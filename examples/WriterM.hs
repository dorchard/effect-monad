{-# LANGUAGE FlexibleInstances #-}

import Data.Monoid

data Writer w a = Writer { runWriter :: (a, w) }

instance Monoid w => Monad (Writer w) where
   return a = Writer (a, mempty)
   (Writer (a, w)) >>= k = let (b, w') = runWriter (k a)
                           in Writer (b, w `mappend` w')

instance Monad (Writer (Maybe a)) where
   return a = Writer (a, Nothing)
   (Writer (a, w)) >>= k = let (b, w') = runWriter (k a)
                           in case w' of 
                                Nothing -> Writer (b, w)
                                Just w' -> Writer (b, Just w')