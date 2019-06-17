module Encoder where

import Numeric.Tools.Integration
import Test.RandomStrings

encode :: String -> Double -> String
encode message radius = 
    let distance = getHelicalLengthOfLetters radius

    in addNoise message distance

decode :: String -> Double -> String
decode message radius = 
    let distance = getHelicalLengthOfLetters radius

    in dropEvery message distance

addNoise :: String -> Int -> String
addNoise xs n =
    if length xs <= 1
    then xs
    else 
        let 
            length = n - 1
            firstChar = take 1 xs
            noise = replicate length '.'
            firstCharEncoded = firstChar ++ noise
            theRestOfTheMessage = addNoise (drop 1 xs) n

        in firstCharEncoded ++ theRestOfTheMessage

getRandomString :: Int -> IO String
getRandomString length = do
    r <- randomString (onlyAlpha randomChar8) length
    return r

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = [ i | (i,c) <- ( zip xs [0..]), (mod c n) == 0]

getCircumference :: Double -> Double
getCircumference diameter = pi * diameter

getAproximateLengthOfFirstArchemdedianSpiral :: Double -> Int
getAproximateLengthOfFirstArchemdedianSpiral radius =
    let lowerLimit = 0
        upperLimit = pi/4
        l x = radius * sqrt((tan x)^2 + 1)
        res = quadRomberg defQuad (lowerLimit, upperLimit) l
        arcLength = 8 * quadBestEst res
    in round arcLength

getHelicalLength :: Double -> Double -> Int
getHelicalLength radius height = 
    let circumference = 2 * pi * radius
        length = sqrt(height^2 + circumference^2)
    
    in round length


getHelicalLengthOfLetters :: Double -> Int
getHelicalLengthOfLetters radius = 
    let distanceBetweenFirstLetterAndNextMatchingLetter = 1
    
    in getHelicalLength radius distanceBetweenFirstLetterAndNextMatchingLetter
