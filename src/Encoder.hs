module Encoder where

import Numeric.Tools.Integration
import Test.RandomStrings

encode :: String -> Double -> IO String
encode message radius = do
    let distance = getHelicalLengthOfLetters radius
        messageAsStringList = map return message
        length = distance - 1
    
    noise <- getRandomString length

    return $ concatMap (\x -> x ++ noise) messageAsStringList

decode :: String -> Double -> String
decode message radius = 
    let distance = getHelicalLengthOfLetters radius

    in dropEvery message distance

getRandomString :: Int -> IO String
getRandomString = randomString (onlyAlpha randomChar8)

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = [ i | (i,c) <- ( zip xs [0..]), (mod c n) == 0]

getHelicalLengthOfLetters :: Double -> Int
getHelicalLengthOfLetters radius = 
    let distanceBetweenFirstLetterAndNextMatchingLetter = 1
    
    in getHelicalLength radius distanceBetweenFirstLetterAndNextMatchingLetter

getHelicalLength :: Double -> Double -> Int
getHelicalLength radius height = 
    let circumference = 2 * pi * radius
        length = sqrt(height^2 + circumference^2)
    
    in round length