module Codec.Picture.Blp.Internal.Convert(
    toPngRepresentable
  ) where

import Codec.Picture
import Codec.Picture.Types

toPngRepresentable :: DynamicImage -> DynamicImage 
toPngRepresentable i = case i of 
  ImageY8 _ -> i 
  ImageY16 _ -> i
  ImageYF p -> ImageRGB16 . convertFloatImage . promoteImage $ p
  ImageYA8 _ -> i
  ImageYA16 _ -> i
  ImageRGB8 _ -> i 
  ImageRGB16 _ -> i
  ImageRGBF p -> ImageRGB16 . convertFloatImage $ p
  ImageRGBA8 _ -> i 
  ImageRGBA16 _ -> i
  ImageYCbCr8 p -> ImageRGB8 . convertImage $ p
  ImageCMYK8 p -> ImageRGBA8 . convertCMYK8Image $ p 
  ImageCMYK16 p -> ImageRGBA16 . convertCMYK16Image $ p

convertFloatImage :: Image PixelRGBF -> Image PixelRGB16
convertFloatImage = pixelMap convert 
  where 
    convert (PixelRGBF rf gf bf) = PixelRGB16 
      (round $ 65535 * rf)
      (round $ 65535 * gf)
      (round $ 65535 * bf)

convertCMYK8Image :: Image PixelCMYK8 -> Image PixelRGBA8 
convertCMYK8Image = pixelMap convert 
  where 
  convert (PixelCMYK8 c m y k) =
      PixelRGBA8 (clampWord8 r) (clampWord8 g) (clampWord8 b) k
    where
    clampWord8 = fromIntegral . max 0 . min 255 . (`div` 255)

    ik :: Int
    ik = fromIntegral k
    r = fromIntegral y * ik 
    g = fromIntegral m * ik 
    b = fromIntegral c * ik 

convertCMYK16Image :: Image PixelCMYK16 -> Image PixelRGBA16 
convertCMYK16Image = pixelMap convert 
  where 
  convert (PixelCMYK16 c m y k) =
      PixelRGBA16 (clampWord16 r) (clampWord16 g) (clampWord16 b) 65535
    where
    clampWord16 = fromIntegral . max 0 . min 65535 . (`div` 65535)

    ik :: Int
    ik = fromIntegral k
    r = fromIntegral y * ik 
    g = fromIntegral m * ik 
    b = fromIntegral c * ik 