import Test.Hspec

import Block1
import Block2
import Block3

testStringSum :: Spec
testStringSum = do
  let runAndCheck :: Maybe Int -> String -> IO ()
      runAndCheck expected test = do
        (stringSum test) `shouldBe` expected

  describe "test string sum" $ do
    it "test two numbers" $ do
      runAndCheck (Just 30) "10 20"

    it "test nothing" $ do
      runAndCheck Nothing "10 20 hi hello this should be nothing ok da"
      
    it "just only words and no integers at all here" $ do
      runAndCheck Nothing "test words"

    it "test empty string" $ do
      runAndCheck (Just 0) ""

    it "test several numbers" $ do
      runAndCheck (Just 55) "1 2 3 4 5 6 7 8 9 10"

    it "test floats" $ do
      runAndCheck Nothing "10.2 15 30"

testEval :: Spec
testEval = do
  let runAndCheck :: Either ArithmeticError Int -> Expr -> IO ()
      runAndCheck expected test = do
        (eval test) `shouldBe` expected

  describe "test eval" $ do
    it "test sum" $ do
      runAndCheck (Right 30) (Sum (Constant 10) (Constant 20))

    it "test sub" $ do
      runAndCheck (Right (-10)) (Sub (Constant 10) (Constant 20))

    it "test mul" $ do
      runAndCheck (Right 200) (Mul (Constant 10) (Constant 20))

    it "test div" $ do
      runAndCheck (Right 2) (Div (Constant 20) (Constant 10))

    it "test div error" $ do
      let denom = (Sub (Constant 10) (Sum (Constant 5) (Constant 5)))
      runAndCheck (Left DivisionByZero) (Div (Constant 20) denom)

    it "test pow" $ do
      runAndCheck (Right 1024) (Pow (Constant 2) (Constant 10))

    it "test pow error" $ do
      let pw = (Sub (Constant 1) (Sum (Constant 5) (Constant 5)))
      runAndCheck (Left NegativePower) (Pow (Constant 20) pw)

testMoving :: Spec
testMoving = do
  describe "test moving" $ do
    it "sample #1" $ do
      moving 4 [1, 5, 3, 8, 7, 9, 6] `shouldBe` 
        [1.0, 3.0, 3.0, 4.25, 5.75, 6.75, 7.5]
    it "sample #2" $ do
      moving 2 [1, 5, 3, 8, 7, 9, 6] `shouldBe`
        [1.0, 3.0, 4.0, 5.5, 7.5, 8.0, 7.5]
    it "test large 10^4 with n = 10^4" $ do
      (moving 10000 (replicate 10000 1 :: [Int])) `shouldBe`
        (replicate 10000 1.0 :: [Double])
    it "test large 10^4 with n = 1" $ do
      (moving 1 [1,2..10000]) `shouldBe`
        ([1,2..10000] :: [Double])

testTree :: Spec
testTree = do
  describe "test laws on tree" $ do
    it "test tree functor laws" $ do
      testTreeFunctorLaws `shouldBe` True
    it "test tree applicative laws" $ do
      testTreeApplicativeLaws `shouldBe` True
    it "test tree foldable" $ do
      testTreeFoldable `shouldBe` True

testNonEmpty :: Spec
testNonEmpty = do
  describe "test laws on non empty" $ do
    it "test non empty functor laws" $ do
      testNonEmptyFunctorLaws `shouldBe` True

    it "test non empty applicative laws" $ do
      testNonEmptyApplicativeLaws `shouldBe` True

    it "test non empty monad laws" $ do
      testNonEmptyMonadLaws `shouldBe` True

    it "test non empty foldable" $ do
      testNonEmptyFoldable `shouldBe` True

testBasicCombinators :: Spec
testBasicCombinators = do
  describe "test basic combinators" $ do
    describe "test ok" $ do
      it "on string" $ do
        runParser ok "hi hello" `shouldBe` (Just ((), "hi hello"))
      it "on list" $ do
        runParser ok ([1, 2] :: [Int]) `shouldBe` (Just ((), [1, 2]))

    it "test eof" $ do
      runParser eof "" `shouldBe` (Just ((), ""))

    describe "test satisfy" $ do
      it "matches first" $ do
        let expected = Just (10, [10, 5])
        let received = runParser (satisfy (10 ==)) [10 :: Int, 10, 5]
        received `shouldBe` expected
      it "does not match first" $ do
        let expected = Nothing
        let received = runParser (satisfy (10 ==)) [20 :: Int, 10, 5]
        received `shouldBe` expected
    
    it "test element" $ do
      let expected = Just ('a', "abc")
      let received = runParser (element 'a') "aabc"
      received `shouldBe` expected

    it "test stream" $ do
      let expected = Just ("aa", "bc")
      let received = runParser (stream "aa") "aabc"
      received `shouldBe` expected

testAdvancedCombinators :: Spec
testAdvancedCombinators = do
  describe "test advanced combinators and parse functions" $ do
    describe "test zero or more" $ do
      it "more" $ do
        let expected = Just ("aa", "bbaa")
        let received = runParser (zeroOrMore (element 'a')) "aabbaa"
        received `shouldBe` expected
      it "zero" $ do
        let expected = Just ("", "bbaa")
        let received = runParser (zeroOrMore (element 'a')) "bbaa"
        received `shouldBe` expected
    
    describe "test one or more" $ do
      it "zero" $ do
        let expected = Nothing
        let received = runParser (oneOrMore (element 'a')) "bbaa"
        received `shouldBe` expected
      it "one or more" $ do
        let expected = Just ("aa", "bbaa")
        let received = runParser (oneOrMore (element 'a')) "aabbaa"
        received `shouldBe` expected
    
    describe "test int parser" $ do
      describe "test zero" $ do
        it "0" $ do
          runParser intParser "0" `shouldBe` Just ("0", "")
        it "-0" $ do
          runParser intParser "-0" `shouldBe` Just ("-0", "")
        it "+0" $ do
          runParser intParser "+0" `shouldBe` Just ("0", "")
      describe "test positive" $ do
        it "1234567890" $ do
          runParser intParser "1234567890" `shouldBe` Just ("1234567890", "")
        it "+1234567890" $ do
          runParser intParser "+1234567890" `shouldBe` Just ("1234567890", "")
      describe "test negative" $ do
        it "-1234567890" $ do
          runParser intParser "-1234567890" `shouldBe` Just ("-1234567890", "")
      describe "test not a number" $ do
        it "empty string" $ do
          runParser intParser "" `shouldBe` Nothing
        it "no digits but letters" $ do
          runParser intParser "awogijaw" `shouldBe` Nothing
        it "letters and digits" $ do
          runParser intParser "bdkjfne123900" `shouldBe` Nothing
      describe "number first and then garbage" $ do
        it "positive without +" $ do
          runParser intParser "123awpfoka" `shouldBe` Just ("123", "awpfoka")
        it "positive with +" $ do
          runParser intParser "+500lolkek" `shouldBe` Just ("500", "lolkek")
        it "negative" $ do
          runParser intParser "-10hi" `shouldBe` Just ("-10", "hi")
      describe "parse int" $ do
        describe "test zero" $ do
          it "0" $ do
            parseInt "0" `shouldBe` Just 0
          it "-0" $ do
            parseInt "-0" `shouldBe` Just 0
          it "+0" $ do
            parseInt "+0" `shouldBe` Just 0  
        describe "positive" $ do
          it "10" $ do
            parseInt "10" `shouldBe` Just 10
          it "+100500" $ do
            parseInt "+100500" `shouldBe` Just 100500
        describe "negative" $ do
          it "-10" $ do
            parseInt "-10" `shouldBe` Just (-10)
        describe "test not a number" $ do
          it "empty string" $ do
            parseInt "" `shouldBe` Nothing
          it "just letters" $ do
            parseInt "waogijawogij" `shouldBe` Nothing
          it "digits and then letters" $ do
            parseInt "100500aowijawg" `shouldBe` Nothing
          it "letters and then digits" $ do
            parseInt "awogijawgo23059" `shouldBe` Nothing
    describe "test parse brackets" $ do
        it "test empty string" $ do
          parseBracketSequence "" `shouldBe` Just ""
        it "test ()" $ do
          parseBracketSequence "()" `shouldBe` Just "()"
        it "test ()()" $ do
          parseBracketSequence "()()" `shouldBe` Just "()()"
        describe "random valid" $ do
          it "random#1" $ do
            parseBracketSequence "(())()()(())" `shouldBe` Just "(())()()(())"
          it "random#2" $ do
            parseBracketSequence "(())(()()()((()())))" 
              `shouldBe` Just "(())(()()()((()())))"
        describe "test invalid" $ do
            it "test )" $ do
              parseBracketSequence ")" `shouldBe` Nothing
            it "test (" $ do
              parseBracketSequence "(" `shouldBe` Nothing
            describe "test negative balance big" $ do
              it "negative balance big #1" $ do
                parseBracketSequence "(()))" `shouldBe` Nothing
              it "negative balance big #1" $ do
                parseBracketSequence "()())()(())" `shouldBe` Nothing

main :: IO ()
main = hspec $ do
  testStringSum
  testEval
  testMoving
  testTree
  testNonEmpty
  testBasicCombinators
  testAdvancedCombinators
