module Main where

import qualified Codec.Picture as CP
import qualified System.IO as SIO
import Data.Either
import qualified Data.Vector.Storable as V

imagePath :: SIO.FilePath
imagePath = "./Sakuya[WhoAmI].jpg"

type Harmonic = (Float, Float) 

countSampleNumber :: Int -> Int -> Int
countSampleNumber scale sampleRate = scale * (sampleRate `div` 1000)

makeSample :: Harmonic -> Int -> Int -> Float
makeSample (amplitude, freq) sampleRate x = amplitude * harmonic
                                            where harmonic = cos (angularFreq * c)
                                                  angularFreq = 2 * pi * freq    
                                                  c = (fromIntegral x) / (fromIntegral sampleRate)    

makeSum :: [Harmonic] -> Int -> Int -> Float
makeSum harmonics sampleRate x = sum samples
                                 where samples = map func harmonics
                                       func harmonic = makeSample harmonic sampleRate x

addFreq :: [Harmonic] -> Int -> Int -> [Float]
addFreq harmonics scale sampleRate = addFreq' harmonics sampleRate numberOfSamples []
                                     where numberOfSamples = countSampleNumber scale sampleRate

addFreq' :: [Harmonic] -> Int -> Int -> [Float] -> [Float]
addFreq' harmonics sampleRate x list = if x==0
                                        then list
                                        else addFreq' harmonics sampleRate (x-1) newList
                                       where newList = value:list
                                             value = makeSum harmonics sampleRate x

{-TODO could be better-}
drawLine :: Int -> Int -> [Float] -> Int -> [Harmonic]
drawLine scale sampleRate line y = map func arraysque
                                   where func value = toHarmonic (fst value) (fromIntegral (snd value))
                                         count = [0..(length line - 1)]
                                         arraysque = zip line count  

draw :: Int -> Int -> [[Float]] -> [Harmonic]
draw scale sampleRate pic = concat (map func arraysque)
                            where func value = drawLine scale sampleRate (fst value) (snd value)
                                  count = [0..(length pic - 1)]
                                  arraysque = zip pic count  
{-TODO-}
getRawAudio :: Int -> Int -> [[Float]] -> [Float]
getRawAudio scale sampleRate pic = addFreq (draw scale sampleRate pic) scale sampleRate

getCleanAudio :: Int -> Int -> [[Float]] -> [Float]
getCleanAudio scale sampleRate pic = map (\x->x/norm) audio
                                     where norm = maximum audio
                                           audio = getRawAudio scale sampleRate pic

toHarmonic :: Float -> Float -> Harmonic
toHarmonic amp freq = (amp, freq)

getDynSize :: CP.DynamicImage -> (Int, Int)
getDynSize img = (dynWidth img, dynHeight img)

dynWidth :: CP.DynamicImage -> Int
dynWidth img = CP.dynamicMap CP.imageWidth img

dynHeight :: CP.DynamicImage -> Int
dynHeight img = CP.dynamicMap CP.imageHeight img

extractPixels :: CP.Image CP.PixelRGB16 ->  [[Float]]
extractPixels img = extractPixels' img width height [] 
                    where width = CP.imageWidth img
                          height = CP.imageHeight img

extractPixels' :: CP.Image CP.PixelRGB16 -> Int -> Int ->  [[Float]] -> [[Float]]
extractPixels' img x y list = if y>0
                                then extractPixels' img x (y-1) (value:list)
                                else list
                              where value = extractPixelsLine img x y []

extractPixelsLine :: CP.Image CP.PixelRGB16 -> Int -> Int ->  [Float] -> [Float]
extractPixelsLine img x y list = if x>0
                                   then extractPixelsLine img (x-1) y (value:list)
                                   else list
                                  where value = getPixel img x y

getPixel img x y = convertPixelToGrayscale (CP.pixelAt img x y)
{-TODO overload-}
convertPixelToGrayscale :: CP.PixelRGB16 -> Float
convertPixelToGrayscale (CP.PixelRGB16 r g b) = 0.299*red + 0.587*green + 0.114*blue
                                               where red = fromIntegral r
                                                     green = fromIntegral g
                                                     blue = fromIntegral b

doJob :: Int -> Int -> CP.Image CP.PixelRGB16 -> [Float]
doJob scale sampleRate img = getCleanAudio scale sampleRate pic
                             where pic = extractPixels img

main = do 
        image <- CP.readImage imagePath
        case image of
            Left err -> putStrLn ("Could not read image: " ++ err)
            Right img -> putStrLn ("Success: " ++ (show (doJob 20 44100 (CP.convertRGB16 img))))
        
