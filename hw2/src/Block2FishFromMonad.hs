{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Block2FishFromMonad
  (
    (>=>)
  , returnFish
  , Block2FishFromMonad.return
  , (Block2FishFromMonad.>>=)
  ) where

class Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b
  return :: a -> m a
  
class MonadFish m where
  returnFish :: a -> m a
  (>=>)      :: (a -> m b) -> (b -> m c) -> a -> m c

instance Block2FishFromMonad.Monad m => MonadFish m where
  returnFish = Block2FishFromMonad.return
  (>=>) f g x = (Block2FishFromMonad.>>=) (f x) g
