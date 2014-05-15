{-# LANGUAGE EmptyDataDecls, TypeFamilies, GADTs #-}

module Control.Effect.Maybe where

import Prelude hiding (Monad(..))

import Control.Effect
import Control.Effect.Cond

{-| Provides an indexed version of the |Maybe| monad -}

data F 
data T 
data U

data IMaybe p a where
    INothing ::               IMaybe F a 
    IJust    :: a          -> IMaybe T a 
    IDyn     :: IMaybe s a -> IMaybe U a -- dynamic partiality

instance Show a => Show (IMaybe p a) where
    show INothing  = "Nothing"
    show (IJust a) = "Just " ++ show a
    show (IDyn a)  = show a

instance Effect IMaybe where
  type Inv IMaybe s t = ()
  type Unit IMaybe = T

  type Plus IMaybe F s = F -- conjunction
  type Plus IMaybe T s = s
  type Plus IMaybe U s = U

  return x = IJust x

  -- static
  (IJust x) >>= k = k x
  INothing  >>= k = INothing

  -- dynamic (statically undecidable)
  (IDyn (IJust a))  >>= k = IDyn (k a)
  (IDyn (INothing)) >>= k = IDyn INothing 
  
instance Cond IMaybe where
    type AltInv IMaybe s t = ()

    type Alt IMaybe T T = T
    type Alt IMaybe F F = F

    type Alt IMaybe F T = U
    type Alt IMaybe T F = U

    -- statically decidable
    ifM True  (IJust x) (IJust y) = IJust x
    ifM False (IJust x) (IJust y) = IJust y
    ifM True  INothing  INothing  = INothing
    ifM False INothing  INothing  = INothing

    -- dynamic (statically undecidable)
    ifM True  INothing  (IJust x) = IDyn INothing
    ifM False INothing  (IJust x) = IDyn (IJust x)
    ifM True  (IJust x) INothing  = IDyn (IJust x)
    ifM False (IJust x) INothing  = IDyn INothing