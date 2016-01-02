module Root.Test.Test1Spec where

import Root.Src.C
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec spec
spec::Spec
spec = do
	describe "Prelude.head" $ do
		it "returns the first element of a list" $ do
			head [23 ..] `shouldBe` (23 :: Int)
		it "returns the first element of another list" $ do
			head [24 ..] `shouldBe` (24 :: Int)
		it "returns the first element of another list 25" $ do
			head [25 ..] `shouldBe` (25 :: Int)
		it "returns the first element of another list 30" $ do
			head [30 ..] `shouldBe` (30 :: Int)
		it "returns the first element of another list 36" $ do
			head [36 ..] `shouldBe` (36 :: Int)
		it "returns the first element of another list 7" $ do
			head [7 ..] `shouldBe` (7 :: Int)
		it "returns the first element of another list 8" $ do
			head [8 ..] `shouldBe` (8 :: Int)
		it "returns the first element of another list 9" $ do
			head [9 ..] `shouldBe` (9 :: Int)
		it "returns the first element of another list 10" $ do
			head [124 ..] `shouldBe` (124 :: Int)
		it "returns the first element of another list 11" $ do
			head [1224 ..] `shouldBe` (1224 :: Int)
		it "returns the first element of another list 12" $ do
			head [12 ..] `shouldBe` (12 :: Int)
		it "returns the first element of another list 13" $ do
			head [13 ..] `shouldBe` (13 :: Int)
