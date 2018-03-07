[![Build Status](https://travis-ci.org/waddlaw/testable-io.svg?branch=master)](https://travis-ci.org/waddlaw/testable-io)

# testable-io

Inspired: [imperative-edsl](https://hackage.haskell.org/package/imperative-edsl)

## Example

```haskell
module Main (main) where

import           Test.FakeIO
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "execFakeIO" $
    it "use getChar" $ do
      execFakeIO act "Haskell" `shouldReturn` "('H','s')\n"
      execFakeIO act "GHC" `shouldReturn` "('G','C')\n"
      execFakeIO act "guchi" `shouldReturn` "('g','c')\n"
      execFakeIO act "BIGMOON" `shouldReturn` "('B','G')\n"
  describe "evalFakeIO" $
    it "use getChar" $ do
      evalFakeIO act "Haskell" `shouldReturn` ('H', 's')
      evalFakeIO act "GHC" `shouldReturn` ('G','C')
      evalFakeIO act "guchi" `shouldReturn` ('g','c')
      evalFakeIO act "BIGMOON" `shouldReturn` ('B','G')

act :: IO (Char, Char)
act = do
  x <- getChar
  _ <- getChar
  y <- getChar
  print (x, y)
  return (x, y)
```

## Restriction

This library is only work on [hspec](http://hspec.github.io/).