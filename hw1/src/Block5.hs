module Block5 (maybeConcat) where

maybeConcat :: [Maybe [a]] -> [a]
maybeConcat elements = mconcat (map unwrapMaybeList elements) where
    unwrapMaybeList :: Maybe [a] -> [a]
    unwrapMaybeList Nothing = mempty
    unwrapMaybeList (Just xs) = xs


