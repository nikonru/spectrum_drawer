module Main where

import System.Environment

import ImageReader (convertImageToWave)

path = "tree.png"

parsePath args = head args
--TODO add command-line interface
--TODO investigate memory overusage for 143x80 png file with dt = 1 sec and df = 240 Hz                   
main :: IO ()
main = do
         args <- getArgs
         --let path = parsePath args
         convertImageToWave path "test.wav" 0.1 240
