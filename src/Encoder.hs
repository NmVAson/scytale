module Encoder where

import Numeric.Tools.Integration

encode :: Int -> Int
encode x = x

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