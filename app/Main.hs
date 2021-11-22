module Main where

import Lib ( someFunc, introduce )
-- import CustomTypes

helloWorld :: IO ()
helloWorld = putStrLn "Hello World"

main :: IO ()
main = do 
    someFunc
    putStrLn "Hello"
    putStrLn "World"
    helloWorld
    helloWorld
    helloWorld
    introduce "Piotr" "Madzia"
