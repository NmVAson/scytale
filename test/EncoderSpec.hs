import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Encoder

main :: IO ()
main = hspec $ do
    describe "Encoder" $ do
        it "should be the identity function" $ do
            encode 1 `shouldBe` 1

        