import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Encoder

main :: IO ()
main = hspec $ do
    describe "Encoder" $ do
        it "should calculate the circumference" $ do 
            let diameter = 2.1
                expectedCircumference = 6.5973445725385655

            getCircumference diameter `shouldBe` expectedCircumference

        it "should caluclate an approximation of the length of the first spiral" $ do
            let radius = 1
                expectedArcLength = 7

            getAproximateLengthOfFirstArchemdedianSpiral radius `shouldBe` expectedArcLength

        it "should calculate the length of a helical curve" $ do 
            let radius = 2.5
                height = 10
                expectedLength = 19
            
            getHelicalLength radius height `shouldBe` expectedLength


        it "should calculate the length of a helical curve of letters around a scytale" $ do 
            let radius = 1
                expectedLength = 6
            
            getHelicalLengthOfLetters radius `shouldBe` expectedLength

        it "should drop every nth letter from string" $ do 
            let n = 6
                testMessage = "d.....o.....g"
                expected = "dog"
            
            dropEvery testMessage n `shouldBe` expected


        it "should return aligned letters" $ do 
            let radius = 1
                testMessage = "d.....o.....g"
                expected = "dog"
            
            decode testMessage radius `shouldBe` expected
            


        