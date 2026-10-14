{-|
Decoding and encoding of BLP1, the texture format of Warcraft III, on top of
JuicyPixels.

BLP1 comes in two flavours: JPEG compressed and uncompressed with a 256
colour palette. Uncompressed textures either carry an alpha plane or not.
A file holds the full size image followed by its mipmaps, each half the size
of the previous one.

Decoding always yields 'ImageRGBA8'. Files from the wild are often slightly
off spec; the decoder accepts the common deviations and, when a mipmap is
damaged, keeps the levels before it.

> Right img <- readBlp "war3mapMap.blp"
> writePng "war3mapMap.png" (convertRGBA8 img)
-}
module Codec.Picture.Blp(
  -- * Decoding
    readBlp
  , readBlpMipmaps
  , decodeBlp
  , decodeBlpMipmaps
  -- * Encoding
  -- | The mipmap count passed to the encoders is the number of levels to
  -- write, mip 0 included. It is clamped to at least one, and asking for
  -- more levels than the image has stops at 1x1. 'mipMapsUpTo' computes it from a minimum side length.
  , writeBlpJpeg
  , writeBlpUncompressedWithAlpha
  , writeBlpUncompressedWithoutAlpha
  , encodeBlpJpeg
  , encodeBlpUncompressedWithAlpha
  , encodeBlpUncompressedWithoutAlpha
  , mipMapsUpTo
  ) where

import Codec.Picture
import Data.Bits ((.&.), shiftR)
import Data.ByteString (ByteString)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as V

import Codec.Picture.Blp.Internal.Convert
import Codec.Picture.Blp.Internal.Data
import Codec.Picture.Blp.Internal.Encoder
import Codec.Picture.Blp.Internal.Parser

-- | Read a BLP file and decode its full size image. IO errors, such as a
-- missing file, are thrown; malformed contents give 'Left'.
readBlp :: FilePath -> IO (Either String DynamicImage)
readBlp fp = decodeBlp `fmap` BS.readFile fp

-- | Read a BLP file and decode the full size image followed by its mipmaps.
-- See 'decodeBlpMipmaps'.
readBlpMipmaps :: FilePath -> IO (Either String [DynamicImage])
readBlpMipmaps fp = decodeBlpMipmaps `fmap` BS.readFile fp

-- | Decode the full size image of a BLP file. The result is 'ImageRGBA8'.
decodeBlp :: ByteString -> Either String DynamicImage
decodeBlp bs = do
  is <- decodeBlpMipmaps bs
  case is of
    i : _ -> Right i
    [] -> Left "No data in BLP"

-- | Decode the full size image followed by its mipmaps, all 'ImageRGBA8'.
--
-- Element @ is always mip level @. Mipmaps are decoded lazily, and the
-- list stops at the first one that is damaged, so a broken mipmap never
-- costs the levels before it. A damaged full size image fails the file.
decodeBlpMipmaps :: ByteString -> Either String [DynamicImage]
decodeBlpMipmaps bs = do
  blp <- parseBlp bs
  let width = fromIntegral $ blpWidth blp
      height = fromIntegral $ blpHeight blp
  case blpExt blp of
    BlpJpeg {..} -> do
      let jpegs = (blpJpegHeader <>) `fmap` blpJpegData
      case jpegs of
        [] -> Left "No JPEG mipmap data in BLP"
        (j0:js) -> do
          -- Mipmap 0 is the full-resolution image; single-image callers
          -- (`decodeBlp`) only ever take `head` of the result. Decoding it
          -- strictly means a failure here still fails the whole file as
          -- before. Tail mipmaps are decoded lazily and the chain stops at the
          -- first one that fails, so a corrupt tail can no longer poison the
          -- main image, and the result stays a prefix of the mip levels
          -- (index i is always level i).
          mip0 <- decodeJpeg j0
          let validPrefix (j:rest) | Right m <- decodeJpeg j = m : validPrefix rest
              validPrefix _ = []
              lazyTail = validPrefix js
              -- The alpha decision is taken once, from the full size mipmap, so that
              -- every mipmap of a texture is decoded the same way. Smaller mipmaps
              -- are often padded out with a flat alpha plane of their own.
              useAlpha = blpAlphaBits blp == 8 && jpegAlphaIsUsable mip0
          Right $ fromBlpJpeg useAlpha <$> (mip0 : lazyTail)

    BlpPaletted {..} -> palettedLevels width height alphaBits blpPalette blpPalettedMipMaps
      where
      -- Alpha depth as declared, if valid for paletted content. A mip 0 too
      -- short to hold that much alpha has none: some tools declare alpha and
      -- then write only the index list.
      declared = case blpAlphaBits blp of
        b | b `elem` [1, 4, 8] -> fromIntegral b
        _ -> 0
      alphaBits = case blpPalettedMipMaps of
        mip0 : _ | BS.length mip0 < levelBytes (width * height) declared -> 0
        _ -> declared

-- | Bytes a paletted mip level of @n@ pixels takes: the index list and the
-- alpha list.
levelBytes :: Int -> Int -> Int
levelBytes n alphaBits = n + (n * alphaBits + 7) `div` 8

-- | Render the mipmaps of a paletted BLP. Level @l@ of a @w@ x @h@ texture is
-- @w / 2^l@ by @h / 2^l@, but never smaller than 1x1. The list stops at the
-- first level whose data is too short for its size, so the result is always a
-- prefix of the mip levels; a short level 0 fails the file.
palettedLevels :: Int -> Int -> Int -> V.Vector PixelRGBA8 -> [ByteString] -> Either String [DynamicImage]
palettedLevels width height alphaBits palette mips
  | null mips = Right []
  | otherwise = case go 0 mips of
      [] -> Left "Mipmap 0 is too short for the image size"
      levels -> Right levels
  where
    go l (mip : rest)
      | BS.length mip >= levelBytes (w * h) alphaBits =
          ImageRGBA8 (generateImage (pixel w (BS.drop (w * h) mip) mip) w h) : go (l + 1) rest
      where
        w = levelSide width l
        h = levelSide height l
    go _ _ = []
    levelSide side l = if side <= 0 then 0 else max 1 (side `shiftR` l)

    -- The palette's fourth byte is padding, not alpha.
    pixel w alphas indices x y = let
      i = y * w + x
      PixelRGBA8 r g b _ = palette V.! fromIntegral (indices `BS.index` i)
      in PixelRGBA8 b g r (alphaAt alphas i)

    -- Alpha values are packed least significant bit first.
    alphaAt alphas i = case alphaBits of
      8 -> alphas `BS.index` i
      4 -> 17 * ((alphas `BS.index` (i `div` 2) `shiftR` (4 * (i `mod` 2))) .&. 0x0f)
      1 -> 255 * ((alphas `BS.index` (i `div` 8) `shiftR` (i `mod` 8)) .&. 1)
      _ -> 255

-- | Number of mip levels, mip 0 included, whose larger side is at least the
-- given length; never less than one. Pass it to the encoders as the mipmap
-- count. With 1 that is the full chain down to 1x1: five levels for 16x16,
-- seven for 64x16. With 4, a 16x16 texture keeps three levels.
mipMapsUpTo :: Int -- ^ Smallest side length to keep (2, 4, 16, ...)
  -> DynamicImage -- ^ Image the mipmaps are made for
  -> Int
mipMapsUpTo minSize img = max 1 . length . takeWhile (>= max 1 minSize) $ iterate (`div` 2) maxSide
  where
    maxSide = max (dynamicMap imageWidth img) (dynamicMap imageHeight img)

-- | Encode with 'encodeBlpJpeg' and write the result to a file.
writeBlpJpeg :: FilePath -> Int -> Int -> DynamicImage -> IO ()
writeBlpJpeg fp quality numMips img = do
  let bs = encodeBlpJpeg quality numMips img
  BSL.writeFile fp bs

-- | Encode with 'encodeBlpUncompressedWithAlpha' and write the result to a file.
writeBlpUncompressedWithAlpha :: FilePath -> Int -> DynamicImage -> IO ()
writeBlpUncompressedWithAlpha fp numMips img = do
  let bs = encodeBlpUncompressedWithAlpha numMips img
  BSL.writeFile fp bs

-- | Encode with 'encodeBlpUncompressedWithoutAlpha' and write the result to a
-- file.
writeBlpUncompressedWithoutAlpha :: FilePath -> Int -> DynamicImage -> IO ()
writeBlpUncompressedWithoutAlpha fp numMips img = do
  let bs = encodeBlpUncompressedWithoutAlpha numMips img
  BSL.writeFile fp bs

-- | Encode as a JPEG compressed BLP. The texture gets an alpha plane when the
-- input image has an alpha channel.
encodeBlpJpeg :: Int -- ^ JPEG quality, 0 to 100
  -> Int -- ^ Number of mip levels to write
  -> DynamicImage
  -> BSL.ByteString
encodeBlpJpeg quality numMips = encodeBlp numMips . toBlpStruct BlpCompressionJPEG quality numMips

-- | Encode as an uncompressed paletted BLP with an alpha plane. An input
-- without alpha gets a fully opaque plane. Colours are quantised to 256.
encodeBlpUncompressedWithAlpha :: Int -- ^ Number of mip levels to write
  -> DynamicImage
  -> BSL.ByteString
encodeBlpUncompressedWithAlpha numMips =
  encodeBlp numMips . toBlpStruct BlpCompressionUncompressed 100 numMips . ImageRGBA8 . convertRGBA8

-- | Encode as an uncompressed paletted BLP without alpha. The alpha channel
-- of the input, if any, is dropped. Colours are quantised to 256.
encodeBlpUncompressedWithoutAlpha :: Int -- ^ Number of mip levels to write
  -> DynamicImage
  -> BSL.ByteString
encodeBlpUncompressedWithoutAlpha numMips =
  encodeBlp numMips . toBlpStruct BlpCompressionUncompressed 100 numMips . ImageRGB8 . convertRGB8
