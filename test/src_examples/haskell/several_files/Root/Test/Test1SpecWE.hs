module Root.Test.Test1SpecWE where

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
