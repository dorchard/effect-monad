{-# LANGUAGE RebindableSyntax, KindSignatures, DataKinds, PolyKinds, NoMonomorphismRestriction, TypeOperators, MultiParamTypeClasses, FlexibleInstances #-}

import Prelude hiding (Monad(..))

import qualified Control.Effect as Eff
import qualified Control.Effect.Reader as R
import Data.Type.Set 

class IMonad (m :: k -> k -> * -> *) where
    return :: a -> m s s a
    (>>=) :: m r s a -> (a -> m s t b) -> m r t b

fail = undefined

data ReaderP s s' a = RWrap (s -> a)

instance IMonad ReaderP where
    return x = RWrap (\_ -> x)
    -- (RWrap r) >>= k = 

data Cons s ss = Cons s ss

ask :: (Getter v t s) => Var v -> ReaderP (Set (Union '[v :-> t] s)) (Set s) t
ask Var = RWrap $ (\x -> get x Var)

class Getter v t s where
    get :: Set (Union '[v :-> t] s) -> Var v -> t 

instance Getter v t '[] where
    get (Ext x Empty) Var = x
   
instance Getter v t '[v :-> t] where
    get (Ext x Empty) Var = x

instance (CmpSymbol x y ~ LT) => Getter x t '[y :-> s] where
    get (Ext x _) Var = x

instance (CmpSymbol x y ~ LT) => Getter x t '[y :-> s] where
    get (Ext x _) Var = x


foo :: Num b => ReaderP (Set '["x" :-> b, "y" :-> b]) (Set '[]) b
foo = do x <- ask (Var :: (Var "x"))
         y <- ask (Var :: (Var "y"))
         z <- ask (Var :: (Var "x"))
         return $ x + y + z