module Main where

import System.Environment


import ImageReader (convertImageToWave)

path = "tree.png"

parsePath args = head args
                   
main :: IO ()
main = do
         args <- getArgs
         --let path = parsePath args
         convertImageToWave path "test.wav" 0.01 240
