{-# LANGUAGE InstanceSigs #-}
module Block2
  (
    Expr(..)
  , ArithmeticError(..)
  , eval
  , Queue(..)
  , empty
  , push
  , top
  , pop
  , moving
  ) where

import Control.Monad.State

-- Задание 1.

data Expr 
  = Constant Int
  | Sum Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr deriving (Eq, Show)

data ArithmeticError = DivisionByZero | NegativePower deriving Eq

instance Show ArithmeticError where
  show :: ArithmeticError -> String
  show NegativePower  = "Negative power is not possible"
  show DivisionByZero = "Division by zero"

apply :: (Int -> Int -> Either ArithmeticError Int) 
      -> Either ArithmeticError Int 
      -> Either ArithmeticError Int
      -> Either ArithmeticError Int
apply f (Right x) (Right y) = f x y
apply _ e@(Left _) _ = e
apply _ _ e@(Left _) = e

eval :: Expr -> Either ArithmeticError Int
eval (Constant x) = Right x
eval (Sum left right) = apply (\x y -> Right (x + y)) (eval left) (eval right)
eval (Sub left right) = apply (\x y -> Right (x - y)) (eval left) (eval right)
eval (Mul left right) = apply (\x y -> Right (x * y)) (eval left) (eval right)
eval (Div left right) = apply customDiv (eval left) (eval right) where
  customDiv :: Int -> Int -> Either ArithmeticError Int
  customDiv _ 0 = Left DivisionByZero
  customDiv x y = Right $ x `div` y
eval (Pow left right) = apply customPow (eval left) (eval right) where
  customPow :: Int -> Int -> Either ArithmeticError Int
  customPow x y = if y < 0 then (Left NegativePower) else (Right $ x ^ y) 

-- Задание 2.

data Queue a = Queue
  { leftStack  :: [a]
  , rightStack :: [a]
  }

empty :: Queue a
empty = Queue [] []

push :: a -> Queue a -> Queue a
push e q = Queue (e:(leftStack q)) (rightStack q)

top :: Queue a -> (a, Queue a)
top q =
  let r = rightStack q in
    if (length r) > 0
      then (head r, Queue (leftStack q) (tail r))
      else 
        let newRight = (reverse $ leftStack q) ++ r in
          (head newRight, Queue [] newRight)

pop :: Queue a -> (a, Queue a)
pop q =
  let r = rightStack q in
    if (length r) > 0 
      then (head r, Queue (leftStack q) (tail r))
      else 
        let newRight = (reverse $ leftStack q) ++ r in
          (head newRight, Queue [] (tail newRight))

size :: Queue a -> Int
size q = (length $ leftStack q) + (length $ rightStack q)

moving :: Int -> [Int] -> [Double]
moving size' elements = fst $ runState (movingState elements) (empty, []) where
  toDouble :: Int -> Double
  toDouble x = fromIntegral x :: Double
  movingState :: [Int] -> State (Queue Int, [Double]) [Double] 
  movingState [] = do
    (_, acc) <- get
    return $ reverse acc
  movingState (x:xs) = do
    (helperStorage, acc) <- get
    if (size helperStorage) == size' 
      then do
        let elementToAdd = toDouble x
            (elementToRemove', newHelpers) = pop helperStorage
            elementToRemove = toDouble elementToRemove'
            previousAverage = head acc
            previousSum = previousAverage * toDouble size'
            newSum = (previousSum - elementToRemove + elementToAdd)
            newAverage = newSum / toDouble size'
        put ((push x newHelpers), (newAverage:acc))
        movingState xs
      else do
          let previousAverage = if (length acc) == 0 then 0.0 else head acc
              previousSum = previousAverage * (toDouble $ length acc)
              newSum = previousSum + toDouble x
              newAverage = newSum / (toDouble $ (length acc) + 1)
          put ((push x helperStorage), (newAverage:acc))
          movingState xs
  
