{-|
Pixel conversions between the BGRA planes of BLP1 JPEG content and
JuicyPixels images.

This is an internal module: its API may change between minor versions.
-}
module Codec.Picture.Blp.Internal.Convert(
    fromBlpJpeg
  , jpegAlphaIsUsable
  , toBlpCMYK8
  ) where

import Codec.Picture
import Codec.Picture.Types (pixelFold)
import Data.Word

-- | Convert a decoded mipmap of a JPEG compressed BLP1 into an image that can
-- be written out directly.
--
-- The four components of a BLP1 JPEG are the raw B, G, R and A planes of the
-- texture. The stream carries no Adobe marker, so a JPEG decoder has nothing
-- to go on and reports it as CMYK; running the generic CMYK conversion over
-- it applies an ink model to data that is not ink, and multiplies every
-- colour channel by the alpha plane. Map the planes straight across instead.
--
-- The 'Bool' says whether the alpha plane should be honoured; see
-- 'jpegAlphaIsUsable'. When it is 'False' the texture is made fully opaque.
-- Any other kind of JPEG (greyscale, YCbCr) is converted to RGBA as usual.
fromBlpJpeg :: Bool -> DynamicImage -> DynamicImage
fromBlpJpeg useAlpha i = case i of
  ImageCMYK8 p ->
    let rgba = blpJpegToRGBA8 p
    in ImageRGBA8 $ if useAlpha then rgba else forceOpaque rgba
  _ -> ImageRGBA8 $ convertRGBA8 i

-- | Whether the alpha plane of a decoded BLP1 JPEG mipmap carries information.
--
-- Warcraft III authoring tools disagree about the alpha plane of a JPEG
-- compressed BLP1. Some set the alpha flag and fill the plane with 0x00,
-- others with 0xFF, and both mean the same thing: the texture is opaque and
-- the plane was never populated. Taking either at face value gives a wrong
-- answer for half the files in the wild, so a plane that is entirely 0x00 or
-- entirely 0xFF is treated as carrying no information at all.
--
-- Any other plane, including one that is uniformly semi-transparent, is used
-- as it stands.
jpegAlphaIsUsable :: DynamicImage -> Bool
jpegAlphaIsUsable i = case i of
  ImageCMYK8 p -> case uniformAlpha (blpJpegToRGBA8 p) of
    Just a -> a /= 0 && a /= 255
    Nothing -> True
  _ -> True

-- | The alpha value every pixel of an image carries, if they all agree. An
-- empty image counts as uniformly opaque.
uniformAlpha :: Image PixelRGBA8 -> Maybe Word8
uniformAlpha img
  | imageWidth img <= 0 || imageHeight img <= 0 = Just 255
  | pixelFold same True img = Just a0
  | otherwise = Nothing
  where
    PixelRGBA8 _ _ _ a0 = pixelAt img 0 0
    same acc _ _ (PixelRGBA8 _ _ _ a) = acc && a == a0

-- | Reinterpret the four planes a JPEG decoder labelled CMYK as the BGRA they
-- actually are. This inverts 'toBlpCMYK8'.
blpJpegToRGBA8 :: Image PixelCMYK8 -> Image PixelRGBA8
blpJpegToRGBA8 = pixelMap convert
  where
  convert (PixelCMYK8 c m y k) = PixelRGBA8 y m c k

forceOpaque :: Image PixelRGBA8 -> Image PixelRGBA8
forceOpaque = pixelMap convert
  where
  convert (PixelRGBA8 r g b _) = PixelRGBA8 r g b 255

-- | Lay out the B, G, R and A planes of a texture as the four components of
-- a BLP1 JPEG. The encoder labels them CMYK only because that is the
-- four-component JPEG it can write; this is the exact inverse of
-- @blpJpegToRGBA8@.
toBlpCMYK8 :: Image PixelRGBA8 -> Image PixelCMYK8
toBlpCMYK8 = pixelMap convert
  where
  convert (PixelRGBA8 r g b a) = PixelCMYK8 b g r a
