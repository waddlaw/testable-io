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
  describe "runFakeIO" $
    it "use getChar" $ do
      runFakeIO act "Haskell" `shouldReturn` (('H', 's'), "('H','s')\n")
      runFakeIO act "GHC" `shouldReturn` (('G','C'), "('G','C')\n")
      runFakeIO act "guchi" `shouldReturn` (('g','c'), "('g','c')\n")
      runFakeIO act "BIGMOON" `shouldReturn` (('B','G'), "('B','G')\n")

act :: IO (Char, Char)
act = do
  x <- getChar
  _ <- getChar
  y <- getChar
  print (x, y)
  return (x, y)