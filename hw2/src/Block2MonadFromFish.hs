{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Block2MonadFromFish 
  (
    (>=>)
  , returnFish
  , Block2MonadFromFish.return
  , (Block2MonadFromFish.>>=)
  ) where

class MonadFish m where
  returnFish :: a -> m a
  (>=>) :: (a -> m b) -> (b -> m c) -> a -> m c

class Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b
  return :: a -> m a

instance MonadFish m => Block2MonadFromFish.Monad m where
  (>>=) x f = ((const x) >=> f) ()
  return = returnFish
