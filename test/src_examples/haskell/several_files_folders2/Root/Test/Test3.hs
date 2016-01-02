module Root.Test.Test3 where

import Root.Src.P1.C
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec spec
spec::Spec
spec = do
	describe "Prelude.head" $ do
		it "returns the first element of a list" $ do
			bar
			head [23 ..] `shouldBe` (25 :: Int)
