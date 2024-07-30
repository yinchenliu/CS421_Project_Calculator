module Main where

import Test.HUnit
import Parser
import Eval



-- Unit tests for individual parsers
testChar :: Test
testChar = TestCase (assertEqual "for (run (char 'a') \"abc\")," [('a', "bc")] (run (char 'a') "abc"))

testDigit :: Test
testDigit = TestCase (assertEqual "for (run digit \"123\")," [('1', "23")] (run digit "123"))

testString :: Test
testString = TestCase (assertEqual "for (run (string \"abc\") \"abcdef\")," [("abc", "def")] (run (string "abc") "abcdef"))

testNat :: Test
testNat = TestCase (assertEqual "for (run nat \"1234 abc\")," [(1234, " abc")] (run nat "1234 abc"))

testInt :: Test
testInt = TestCase (assertEqual "for (run int \"-1234 abc\")," [(-1234, " abc")] (run int "-1234 abc"))

testFloat :: Test
testFloat = TestCase (assertEqual "for (run float \"-123.45 abc\")," [(-123.45, " abc")] (run float "-123.45 abc"))

unitTests :: Test
unitTests = TestList [ TestLabel "testChar" testChar
                     , TestLabel "testDigit" testDigit
                     , TestLabel "testString" testString
                     , TestLabel "testNat" testNat
                     , TestLabel "testInt" testInt
                     , TestLabel "testFloat" testFloat
                     ]

-- Feature tests for higher-level parsers
testExprSimple :: Test
testExprSimple = TestCase (assertEqual "for (eval \"2+3\")," 5.0 (eval "2+3"))

testExprComplex :: Test
testExprComplex = TestCase (assertEqual "for (eval \"2+3*4\")," 14.0 (eval "2+3*4"))

testExprParens :: Test
testExprParens = TestCase (assertEqual "for (eval \"2*(3+4)\")," 14.0 (eval "2*(3+4)"))

testExprNested :: Test
testExprNested = TestCase (assertEqual "for (eval \"(2+3)*(4-1)\")," 15.0 (eval "(2+3)*(4-1)"))

testExprNegative :: Test
testExprNegative = TestCase (assertEqual "for (eval \"2*-3\")," (-6.0) (eval "2*-3"))

testExprDiv :: Test
testExprDiv = TestCase (assertEqual "for (eval \"6/3\")," 2.0 (eval "6/3"))

testExprFloat :: Test
testExprFloat = TestCase (assertEqual "for (eval \"2.5+3.5\")," 6.0 (eval "2.5+3.5"))

featureTests :: Test
featureTests = TestList [ TestLabel "testExprSimple" testExprSimple
                        , TestLabel "testExprComplex" testExprComplex
                        , TestLabel "testExprParens" testExprParens
                        , TestLabel "testExprNested" testExprNested
                        , TestLabel "testExprNegative" testExprNegative
                        , TestLabel "testExprDiv" testExprDiv
                        , TestLabel "testExprFloat" testExprFloat
                        ]

-- Larger tests for more complex expressions
testExprLarge1 :: Test
testExprLarge1 = TestCase (assertEqual "for (eval \"2+3*4-5/5\")," 13.0 (eval "2+3*4-5/5"))

testExprLarge2 :: Test
testExprLarge2 = TestCase (assertEqual "for (eval \"2*(3+4*2)-10/4\")," 19.5 (eval "2*(3+4*2)-10/4"))

testExprLarge3 :: Test
testExprLarge3 = TestCase (assertEqual "for (eval \"((2+3)*4-2*(3-1))/2\")," 8.0 (eval "((2+3)*4-2*(3-1))/2"))

testExprLarge4 :: Test
testExprLarge4 = TestCase (assertEqual "for (eval \"3*(2+(5-3)*(2+2))\")," 30.0 (eval "3*(2+(5-3)*(2+2))"))

testExprComplexFloat1 :: Test
testExprComplexFloat1 = TestCase (assertEqual "for (eval \"3.5*2+(4.2-1.2)/2\")," 8.5 (eval "3.5*2+(4.2-1.2)/2"))

testExprComplexFloat2 :: Test
testExprComplexFloat2 = TestCase (assertEqual "for (eval \"3*4/11*(3-3)\")," 0.0 (eval "3*4/11*(3-3)"))

testExprComplexFloat3 :: Test
testExprComplexFloat3 = TestCase (assertEqual "for (eval \"2*3/4-3.5*(2.2-1.4)\")," (-1.3) (eval "2*3/4-3.5*(2.2-1.4)"))

largerTests :: Test
largerTests = TestList [ TestLabel "testExprLarge1" testExprLarge1
                       , TestLabel "testExprLarge2" testExprLarge2
                       , TestLabel "testExprLarge3" testExprLarge3
                       , TestLabel "testExprLarge4" testExprLarge4
                       , TestLabel "testExprComplexFloat1" testExprComplexFloat1
                       , TestLabel "testExprComplexFloat2" testExprComplexFloat2
                       , TestLabel "testExprComplexFloat3" testExprComplexFloat3
                       ]

main :: IO ()
main = do
    _ <- runTestTT unitTests
    _ <- runTestTT featureTests
    _ <- runTestTT largerTests
    return ()