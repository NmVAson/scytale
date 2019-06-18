module Main where

import Encoder

main :: IO ()
main = do
    putStrLn "Hello, send me a message!"  
    message <- getLine

    encodedMessage <- encode message 1
    putStrLn ("Encoded: " ++ encodedMessage)
    
    let decodedMessage = decode encodedMessage 1
    putStrLn ("And here it is decoded again: " ++ decodedMessage) 
