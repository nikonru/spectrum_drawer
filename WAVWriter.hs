module WAVWriter where

import Data.WAVE
import Data.Int

type MonotonicImage = [[Double]]

getImageWidth :: MonotonicImage -> Int
getImageWidth image = length (head image)

getImageHeight :: MonotonicImage -> Int
getImageHeight image = length image

makeHeader :: Int -> WAVEHeader
makeHeader samplingFrequency = WAVEHeader numChannels frameRate bitsPerSample frames
                               where numChannels = 1
                                     frameRate = samplingFrequency
                                     bitsPerSample = 8 
                                     frames = Nothing

image1 :: MonotonicImage
image1 = [[0.5,0,0,0,0,0,0.2],
          [0,0.5,0,0,0,0,0.1],
          [0.5,0,0,0,0,0.3,0]]

writeAudio :: String -> MonotonicImage -> Double -> Int -> IO()
writeAudio name image dt df = putWAVEFile name wave
                              where wave = WAVE header samples
                                    samples = samplesToOneChannelSamples (draw samplingFrequency dt df image) 
                                    samplingFrequency = 2 * (getImageWidth image) * df
                                    header = makeHeader samplingFrequency

makeSample :: Int -> Int -> Int -> Double -> WAVESample
makeSample samplingFrequency n f magnitude = doubleToSample dsample
                                             where t = fromIntegral n
                                                   sf = fromIntegral samplingFrequency 
                                                   w = (fromIntegral f)*(2*pi)
                                                   dsample = magnitude * (sin (w * t/sf))

makeSampleLine :: Int -> Double -> Int -> Double -> [WAVESample]
makeSampleLine samplingFrequency t f magnitude = map (\n -> makeSample samplingFrequency n f magnitude) samples
                                                 where samples = [0..n]
                                                       n = ceiling (fromIntegral (samplingFrequency) * t)

uniteSampleLines :: [WAVESample] -> [WAVESample] -> [WAVESample]
uniteSampleLines s1 s2 = zipWith (\a b -> doubleToSample ((sampleToDouble a) + (sampleToDouble b))) s1 s2

samplesToOneChannelSamples :: [WAVESample] -> WAVESamples
samplesToOneChannelSamples s = map (\d -> [d]) s

prepareImage :: MonotonicImage -> MonotonicImage
prepareImage image = map (\line -> map divide line) rotatedImage
                     where rotatedImage = rotateImageRight image
                           k = 255 * fromIntegral (getImageHeight rotatedImage) 
                           divide n = n/k

rotateImageRight :: MonotonicImage -> MonotonicImage
rotateImageRight image = map reverse rotatedImage
                         where rotatedImage = map f indexes 
                               indexes = [0..((getImageWidth image) - 1)]
                               f n = do
                                       line <- image
                                       let pixel = line !! n
                                       return pixel 

draw :: Int -> Double -> Int -> MonotonicImage -> [WAVESample]
draw samplingFrequency dt df image = draw' samplingFrequency dt df preparedImage
                                     where preparedImage = prepareImage image

draw' :: Int -> Double -> Int -> MonotonicImage -> [WAVESample]
draw' samplingFrequency dt df [] = []
draw' samplingFrequency dt df (line:lines) = lineSamples
                                               where lineSamples = (drawLine samplingFrequency dt df line) ++ nextLine
                                                     nextLine = (draw' samplingFrequency dt df lines)

drawLine :: Int ->  Double -> Int -> [Double] -> [WAVESample]
drawLine samplingFrequency t df line = foldl1 uniteSampleLines sampleLines
                                       where sampleLines = getLineSamples samplingFrequency t df line 

getLineSamples :: Int -> Double -> Int -> [Double] -> [[WAVESample]]
getLineSamples samplingFrequency t df line =  do 
                                                pixelWIthIndex <- lineIndexed
                                                let pixel = fst pixelWIthIndex
                                                let index = snd pixelWIthIndex
                                                let sample = makeSampleLine samplingFrequency t (index*df) pixel
                                                return sample
                                              where lineIndexed = zip line [1..l]
                                                    l = length line




