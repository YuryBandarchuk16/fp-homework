module Task2
    (
      Neg
    , doubleNeg
    , excludedNeg
    , pierce
    ) where

import Data.Void (Void)

type Neg a = a -> Void

-- Тип не заселяем. В этом уверен на сто процентов!
doubleNeg :: a -> Neg (Neg a)
doubleNeg _ = undefined

-- Тип не заселяем.
excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg = undefined

-- Тип не заселяем.
pierce :: ((a -> b) -> a) -> a
pierce = undefined

-- Остальное не успел заселить :(
