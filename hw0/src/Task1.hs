{-# LANGUAGE TypeOperators #-}
module Task1
    (
      distributivity
    , associator
    , eitherAssoc
    ) where

distributivity :: Either a (b, c) -> (Either a b, Either a c)
distributivity (Left ta)        = (Left ta,  Left ta)
distributivity (Right (tb, tc)) = (Right tb, Right tc)

associator :: (a, (b, c)) -> ((a, b), c)
associator (ta, (tb, tc)) = ((ta, tb), tc)

type (<->) a b = (a -> b, b -> a)

-- Тип не заселяем.
eitherAssoc :: Either a (Either b c) <-> Either (Either a b) c
eitherAssoc = undefined
