{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Block2JoinFromFish 
  (
   (>=>)
  , returnFish
  , returnJoin
  , join
  ) where

class MonadFish m where
  returnFish :: a -> m a
  (>=>)      :: (a -> m b) -> (b -> m c) -> a -> m c

class MonadJoin m where
  returnJoin :: a -> m a
  join       :: m (m a) -> m a

instance MonadFish m => MonadJoin m where
  returnJoin = returnFish
  join x = ((const x >=>) id) ()
