{-|
Pixel conversions between the BGRA planes of BLP1 JPEG content and
JuicyPixels images.

This is an internal module: its API may change between minor versions.
-}
module Codec.Picture.Blp.Internal.Convert(
    fromBlpJpeg
  , jpegAlphaIsUsable
  , toBlpCMYK8
  , opaqueRGBA8
  ) where

import Codec.Picture
import Data.Bits ((.&.))
import Data.Word

import qualified Data.Vector.Storable as VS

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
  ImageCMYK8 p -> ImageRGBA8 $ swapRedBlue (not useAlpha) p
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
  ImageCMYK8 p
    | VS.null planes -> False
    | otherwise -> varies 7 || (a0 /= 0 && a0 /= 255)
    where
      -- The fourth component of every pixel; stops at the first that differs.
      planes = imageData p
      a0 = VS.unsafeIndex planes 3
      varies j
        | j >= VS.length planes = False
        | VS.unsafeIndex planes j /= a0 = True
        | otherwise = varies (j + 4)
  _ -> True

-- | Reinterpret the four planes a JPEG decoder labelled CMYK as the BGRA they
-- actually are, optionally making every pixel opaque. This inverts
-- 'toBlpCMYK8'.
swapRedBlue :: Bool -> Image PixelCMYK8 -> Image PixelRGBA8
swapRedBlue opaque img = Image (imageWidth img) (imageHeight img) (swapBytes opaque (imageData img))

-- | Swap the first and third byte of every four, optionally setting the
-- fourth to 255.
swapBytes :: Bool -> VS.Vector Word8 -> VS.Vector Word8
swapBytes opaque src = VS.generate (VS.length src) $ \i -> case i .&. 3 of
  0 -> VS.unsafeIndex src (i + 2)
  2 -> VS.unsafeIndex src (i - 2)
  3 | opaque -> 255
  _ -> VS.unsafeIndex src i

-- | Set the alpha of every pixel to 255.
opaqueRGBA8 :: Image PixelRGBA8 -> Image PixelRGBA8
opaqueRGBA8 img = img { imageData = VS.imap (\i v -> if i .&. 3 == 3 then 255 else v) (imageData img) }

-- | Lay out the B, G, R and A planes of a texture as the four components of
-- a BLP1 JPEG. The encoder labels them CMYK only because that is the
-- four-component JPEG it can write; this is the exact inverse of
-- 'swapRedBlue'.
toBlpCMYK8 :: Image PixelRGBA8 -> Image PixelCMYK8
toBlpCMYK8 img = Image (imageWidth img) (imageHeight img) (swapBytes False (imageData img))
