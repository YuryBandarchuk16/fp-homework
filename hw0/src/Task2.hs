module Task2
    (
      Neg
    , doubleNeg
    , excludedNeg
    , pierce
    , doubleNegElim
    , thirdNegElim
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

-- Тип не заселяем.
doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined

-- Тип не заселяем.
thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim = undefined
