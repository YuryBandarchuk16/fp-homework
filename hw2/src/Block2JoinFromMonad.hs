{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Block2JoinFromMonad
  (
    (>=>)
  , returnFish
  , Block2JoinFromMonad.return
  , (Block2JoinFromMonad.>>=)
  ) where

class Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b
  return :: a -> m a
  
class MonadJoin m where
  returnJoin :: a -> m a
  join       :: m (m a) -> m a

instance Block2JoinFromMonad.Monad m => MonadJoin m where
  returnJoin = Block2JoinFromMonad.return
  join x = x (Block2JoinFromMonad.>>=) id
