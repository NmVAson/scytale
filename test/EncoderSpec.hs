import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Encoder

main :: IO ()
main = hspec $ do
    describe "Encoder" $ do
        it "should be the identity function" $ do
            encode 1 `shouldBe` 1

        it "should calculate the circumference" $ do 
            let diameter = 2.1
                expectedCircumference = 6.5973445725385655

            getCircumference diameter `shouldBe` expectedCircumference

        it "should caluclate an approximation of the length of the first spiral" $ do
            let radius = 1
                expectedArcLength = 7

            getAproximateLengthOfFirstSpiral radius `shouldBe` expectedArcLength


        