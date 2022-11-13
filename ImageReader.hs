module ImageReader where
import WAVWriter (writeAudio, MonotonicImage)

import Codec.Picture
import Codec.Picture.Types (convertPixel, dropTransparency, promotePixel)


convertImageToWave path name dt df = do
                                       image <- readImage path 
                                       case image of
                                         --Right img -> putStrLn (show (convertDynamicImage img))
                                         Right img -> writeAudio name (convertDynamicImage img) dt df
                                         Left err -> putStrLn ("Failed due: " ++ err)

convertDynamicImage :: DynamicImage -> MonotonicImage          
convertDynamicImage img = convert img
                          where convert = convertImageToGreyscale . convertImage . convertRGB8  


convertImageToList img y = do
                            x <- d
                            let pixel = pixelAt img x y
                            return pixel
                          where d = [0..((imageWidth img)-1)]

convertImage img = do
                     y <- d
                     let line = convertImageToList img y
                     return line
                   where d = [0..((imageHeight img)-1)]

convertImageToGreyscale imgList = map f imgList 
                                  where f line = map g line
                                        g n = convertPixelToGreyscale n


class ConvertableToGreyScale a where
  convertPixelToGreyscale :: a -> Double

instance ConvertableToGreyScale PixelRGB8 where
  convertPixelToGreyscale (PixelRGB8 r0 g0 b0) = 0.299*r + 0.587*g + 0.114*b
                                            where r = fromIntegral r0
                                                  g = fromIntegral g0
                                                  b = fromIntegral b0

instance ConvertableToGreyScale PixelRGB16 where
  convertPixelToGreyscale (PixelRGB16 r0 g0 b0) = 0.299*r + 0.587*g + 0.114*b
                                             where r = fromIntegral r0
                                                   g = fromIntegral g0
                                                   b = fromIntegral b0


