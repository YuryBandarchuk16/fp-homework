module Task3
    (
      s
    , k
    , i
    , identity
    , composition
    , contraction
    , permutation
    ) where

s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

k :: a -> b -> a
k x _ = x

i :: a -> a
i = s k k

identity :: a -> a
identity = i

composition :: (a -> b) -> (c -> a) -> c -> b
composition =  let p  = s (k s) $ s (k k) $ s (k s) $ s (k k) $ i
                   p' = k s
                   q' = s (k k) $ i
                   q  = k $ s (s p' q') $ k $ i in
                    s p q

contraction :: (a -> a -> b) -> a -> b
contraction = let p' = k s
                  q' = s (k k) i
                  p  = s (k s) $ s (s p' q') $ k i
                  q  = k i in
                    s p q

permutation :: (a -> b -> c) -> b -> a -> c
permutation = let p' = s (k s) $ s (k k) i
                  q' = k i
                  p  = s (k s) $ s (k k) $ s (k s) $ s p' q'
                  q  = k $ s (k k) i in
                     s p q
