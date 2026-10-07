module Main where

import Test.Tasty
import qualified Testing.Interpreter as I
import qualified Testing.Parser as P

main :: IO ()
main = defaultMain $
  testGroup "All tests"
    [
      P.tests,
      I.tests
    ]