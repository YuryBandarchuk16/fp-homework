module Task2 (Neg, doubleNeg, excludedNeg, pierce, doubleNegElim, thirdNegElim) where

import Data.Void (Void)

type Neg a = a -> Void

doubleNeg :: a -> Neg (Neg a)
doubleNeg = undefined

excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg = undefined

-- Тип не заселяем.
pierce :: ((a -> b) -> a) -> a 
pierce _ = undefined

doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined

thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim = undefined