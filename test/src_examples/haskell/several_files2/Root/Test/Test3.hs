module Root.Test.Test3 where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec spec
spec::Spec
spec = do
	describe "Prelude.head" $ do
		it "returns the first element of a list" $ do
			head [23 ..] `shouldBe` (25 :: Int)
		it "returns the first element of a list 2" $ do
			head [23 ..] `shouldBe` (25 :: Int)
		it "returns the first element of a list 3" $ do
			head [23 ..] `shouldBe` (25 :: Int)
		it "returns the first element of a list 4" $ do
			head [23 ..] `shouldBe` (25 :: Int)
