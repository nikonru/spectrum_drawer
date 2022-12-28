module Main where

import System.Environment
import ImageReader (convertImageToWave)

parsePath args = head args
parseName args = if length processedArgs == 0
                   then "image.wav"
                   else head processedArgs
                 where processedArgs = drop 1 args

parseDt args = if length processedArgs == 0
                 then 0.05
                 else read $ head processedArgs
               where processedArgs = drop 1 $ tail args

parseDf args = if length processedArgs == 0
                 then 250
                 else read $ head processedArgs
               where processedArgs = drop 1 $ drop 1 $ drop 1 args
                   
main :: IO ()
main = do
         args <- getArgs
         let path = parsePath args
         let name = parseName args
         let dt = parseDt args
         let df = parseDf args
         convertImageToWave path name dt df
