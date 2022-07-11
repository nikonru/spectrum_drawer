module Main where

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

main = print (show (getCleanAudio 20 44100 [[1,1,1],[2,3,4]]))
