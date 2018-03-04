module Codec.Picture.Blp.Internal.Convert(
    toPngRepresentable
  , toBlpUncompressable
  , toBlpCMYK8
  ) where

import Codec.Picture
import Codec.Picture.Types
import Data.Word

toPngRepresentable :: DynamicImage -> DynamicImage
toPngRepresentable i = case i of
  ImageY8 _ -> i
  ImageY16 _ -> i
  ImageYF p -> ImageRGB16 . convertFloatImage16 . promoteImage $ p
  ImageYA8 _ -> i
  ImageYA16 _ -> i
  ImageRGB8 _ -> i
  ImageRGB16 _ -> i
  ImageRGBF p -> ImageRGB16 . convertFloatImage16 $ p
  ImageRGBA8 _ -> i
  ImageRGBA16 _ -> i
  ImageYCbCr8 p -> ImageRGB8 . convertImage $ p
  ImageCMYK8 p -> ImageRGBA8 . convertCMYK8Image $ p
  ImageCMYK16 p -> ImageRGBA16 . convertCMYK16Image $ p

toBlpUncompressable :: DynamicImage -> Image PixelRGBA8
toBlpUncompressable i = case i of
  ImageY8 p -> toBlpRGB8 (promoteImage p :: Image PixelRGB8)
  ImageY16 p -> toBlpRGB8 (dropBits (promoteImage p :: Image PixelRGB16) :: Image PixelRGB8)
  ImageYF p -> toBlpRGB8 (convertFloatImage8 (promoteImage p :: Image PixelRGBF) :: Image PixelRGB8)
  ImageYA8 p -> toBlpRGBA8 (promoteImage p :: Image PixelRGBA8)
  ImageYA16 p -> toBlpRGBA8 (dropBitsA (promoteImage p :: Image PixelRGBA16) :: Image PixelRGBA8)
  ImageRGB8 p -> toBlpRGB8 p
  ImageRGB16 p -> toBlpRGB8 (dropBits p :: Image PixelRGB8)
  ImageRGBF p -> toBlpRGB8 (convertFloatImage8 p :: Image PixelRGB8)
  ImageRGBA8 p -> toBlpRGBA8 p
  ImageRGBA16 p -> toBlpRGBA8 (dropBitsA p :: Image PixelRGBA8)
  ImageYCbCr8 p -> toBlpRGB8 (convertImage p :: Image PixelRGB8)
  ImageCMYK8 p -> toBlpRGB8 (convertImage p :: Image PixelRGB8)
  ImageCMYK16 p -> toBlpRGB8 (dropBits (convertImage p :: Image PixelRGB16) :: Image PixelRGB8)

dropBits :: Image PixelRGB16 -> Image PixelRGB8
dropBits = pixelMap $ \(PixelRGB16 r g b) -> PixelRGB8 (f r) (f g) (f b)
  where
    f :: Word16 -> Word8
    f x = round $ 255 * (fromIntegral x :: Double) / 65535

dropBitsA :: Image PixelRGBA16 -> Image PixelRGBA8
dropBitsA = pixelMap $ \(PixelRGBA16 r g b a) -> PixelRGBA8 (f r) (f g) (f b) (f a)
  where
    f :: Word16 -> Word8
    f x = round $ 255 * (fromIntegral x :: Double) / 65535

convertFloatImage8 :: Image PixelRGBF -> Image PixelRGB8
convertFloatImage8 = pixelMap convert
  where
    convert (PixelRGBF rf gf bf) = PixelRGB8
      (round $ 255 * rf)
      (round $ 255 * gf)
      (round $ 255 * bf)

convertFloatImage16 :: Image PixelRGBF -> Image PixelRGB16
convertFloatImage16 = pixelMap convert
  where
    convert (PixelRGBF rf gf bf) = PixelRGB16
      (round $ 65535 * rf)
      (round $ 65535 * gf)
      (round $ 65535 * bf)

convertCMYK8Image :: Image PixelCMYK8 -> Image PixelRGBA8
convertCMYK8Image = pixelMap convert
  where
  clampWord8 = fromIntegral . max 0 . min 255 . (`div` 256)
  convert (PixelCMYK8 c m y k) =
      PixelRGBA8 (clampWord8 r) (clampWord8 g) (clampWord8 b) k
    where
    ik :: Int
    ik = fromIntegral k
    r = fromIntegral y * ik
    g = fromIntegral m * ik
    b = fromIntegral c * ik

toBlpCMYK8 :: Image PixelRGBA8 -> Image PixelCMYK8
toBlpCMYK8 = pixelMap convert
  where
  clampWord8 = fromIntegral . max 0 . min 255
  convert (PixelRGBA8 r g b a) = PixelCMYK8 (clampWord8 c) (clampWord8 m) (clampWord8 y) a
    where
    ik = fromIntegral a :: Double
    c, m, y :: Int
    c = round $ 256 * fromIntegral b / ik
    m = round $ 256 * fromIntegral g / ik
    y = round $ 256 * fromIntegral r / ik

toBlpRGBA8 :: Image PixelRGBA8 -> Image PixelRGBA8
toBlpRGBA8 = pixelMap convert
  where
  convert (PixelRGBA8 r g b a) = PixelRGBA8 b g r a

toBlpRGB8 :: Image PixelRGB8 -> Image PixelRGBA8
toBlpRGB8 = pixelMap convert
  where
  convert (PixelRGB8 r g b) = PixelRGBA8 b g r 0

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
