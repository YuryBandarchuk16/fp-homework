{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Block3
  (
    Parser(..)
  , ok
  , ok'
  , eof
  , eof'
  , satisfy
  , element
  , stream
  , zeroOrMore
  , oneOrMore
  , (<|>)
  , fmap
  , return
  , (>>=)
  , validateEOFAndExtractResult
  , intParser
  , parseInt
  , manyCorrectBracketSequence
  , parseBracketSequence
  ) where

import Control.Applicative

-- Задание 1.

data Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
  fmap :: (forall a. forall b. (a -> b) -> Parser s a -> Parser s b)
  fmap f (Parser runP1) = Parser runP2
    where 
        runP2 x = extract (runP1 x)
        extract Nothing = Nothing
        extract (Just (output, rest)) = Just ((f output), rest)

instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure x = Parser (\s -> (Just (x, s)))
  
  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  (<*>) (Parser runFP) p1 = Parser runP2
    where
      runP2 x = case (runFP x) of
        Nothing -> Nothing
        Just (f, rest) -> extract f (runParser p1 rest)
      extract _ Nothing = Nothing
      extract f (Just (output, rest)) = Just ((f output), rest)

instance Monad (Parser s) where
  return :: a -> Parser s a
  return = pure
  
  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  (>>=) (Parser runP1) f = Parser runP2
    where 
      runP2 x = case (runP1 x) of
        Nothing -> Nothing
        Just (output, rest) -> extractParseFunction (f output) rest
      extractParseFunction (Parser parseFunction) = parseFunction

instance Alternative (Parser s) where
  empty :: Parser s a
  empty = Parser (\_ -> Nothing)
  (<|>) :: Parser s a -> Parser s a -> Parser s a
  (<|>) (Parser runP1) (Parser runP2) = Parser runP3 where 
    runP3 x = case (runP1 x) of
      Nothing -> runP2 x
      result -> result

-- Задание 2.

ok' :: forall a. forall s. a -> Parser s a
ok' a = Parser (\s -> (Just (a, s)))

ok :: Parser s ()
ok = ok' ()

eof' :: forall a. forall s. a -> Parser s a
eof' x = Parser checkEOF where
  checkEOF :: [s] -> Maybe (a, [s])
  checkEOF [] = Just (x, [])
  checkEOF _ = Nothing

eof :: Parser s ()
eof = eof' ()

satisfy :: forall a. (a -> Bool) -> Parser a a
satisfy predicate = Parser checkPredicate where
  checkPredicate :: [a] -> Maybe (a, [a])
  checkPredicate [] = Nothing
  checkPredicate (x:xs) = if predicate x 
    then Just (x, xs)
    else Nothing

element :: forall a. Eq a => a -> Parser a a
element c = satisfy (c ==)

stream :: forall a. Eq a => [a] -> Parser a [a]
stream s = Parser (checkEqualPrefix s) where
  checkEqualPrefix :: [a] -> [a] -> Maybe ([a], [a])
  checkEqualPrefix [] rest = Just (s, rest)
  checkEqualPrefix (c:cs) (x:xs) =
    if c == x 
      then checkEqualPrefix cs xs
      else Nothing
  checkEqualPrefix _ [] = Nothing

-- Задание 3.

isDigit :: Char -> Bool
isDigit c = case c of
  '0' -> True
  '1' -> True
  '2' -> True
  '3' -> True
  '4' -> True
  '5' -> True
  '6' -> True
  '7' -> True
  '8' -> True
  '9' -> True
  _   -> False

isPlus :: Char -> Bool
isPlus '+' = True
isPlus _   = False

isMinus :: Char -> Bool
isMinus '-' = True
isMinus _   = False

zeroOrMore :: Parser s a -> Parser s [a]
zeroOrMore p@(Parser runP1) = Parser runP2 where
  runP2 x = case (runP1 x) of
    Nothing -> Just ([], x)
    Just (output, rest) -> extractResult output rest
  extractResult output rest = case runParser (zeroOrMore p) rest of
    Nothing -> Just (pure output, rest)
    Just (newOutput, newRest) -> Just (output:newOutput, newRest)

oneOrMore :: Parser s a -> Parser s [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

intParser :: Parser Char String
intParser =   ((satisfy isPlus) *> digitP) 
          <|> ((:) <$> (satisfy isMinus) <*> digitP)
          <|> digitP where
  digitP = oneOrMore (satisfy isDigit)

parseInt :: String -> Maybe Int
parseInt s = read <$> validateEOFAndExtractResult intParser s

parseBracketSequence :: String -> Maybe String
parseBracketSequence s =
  validateEOFAndExtractResult manyCorrectBracketSequence s

manyCorrectBracketSequence :: Parser Char String
manyCorrectBracketSequence = concat <$> zeroOrMore correctBracketSequence

correctBracketSequence :: Parser Char String
correctBracketSequence = 
  (++) <$> ((:) <$> (element '(') <*> manyCorrectBracketSequence) 
    <*> (stream ")")

validateEOFAndExtractResult :: Parser s a -> [s] -> Maybe a
validateEOFAndExtractResult p s = validateAndGet $ runParser p s where
  validateAndGet (Just (result, rest)) = fst <$> (runParser (eof' result) rest) 
  validateAndGet Nothing = Nothing
