{-# LANGUAGE InstanceSigs #-}
module Bonus
  (
    Cont(..)
  ) where

newtype Cont r a = Cont { runCont :: (a -> r) -> r }

instance Functor (Cont r) where
  fmap :: (a -> b) -> Cont r a -> Cont r b
  fmap f (Cont runCont1) = Cont runCont2 where
    runCont2 someFunc = runCont1 $ someFunc . f
