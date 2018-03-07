module Main (main) where

import           Test.FakeIO
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "execFakeIO" $
    it "use getChar" $ do
      execFakeIO act "Haskell" `shouldReturn` "'H'\n"
      execFakeIO act "GHC" `shouldReturn` "'G'\n"
      execFakeIO act "guchi" `shouldReturn` "'g'\n"
      execFakeIO act "BIGMOON" `shouldReturn` "'B'\n"
  describe "evalFakeIO" $
    it "use getChar" $ do
      evalFakeIO act "Haskell" `shouldReturn` ('H', 's')
      evalFakeIO act "GHC" `shouldReturn` ('G','C')
      evalFakeIO act "guchi" `shouldReturn` ('g','c')
      evalFakeIO act "BIGMOON" `shouldReturn` ('B','G')
  describe "runFakeIO" $
    it "use getChar" $ do
      runFakeIO act "Haskell" `shouldReturn` (('H', 's'), "'H'\n")
      runFakeIO act "GHC" `shouldReturn` (('G','C'), "'G'\n")
      runFakeIO act "guchi" `shouldReturn` (('g','c'), "'g'\n")
      runFakeIO act "BIGMOON" `shouldReturn` (('B','G'), "'B'\n")

act :: IO (Char, Char)
act = do
  x <- getChar
  print x
  _ <- getChar
  y <- getChar
  return (x, y)