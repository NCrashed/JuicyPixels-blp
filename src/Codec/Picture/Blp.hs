module Codec.Picture.Blp(
    readBlp
  , readBlpMipmaps
  , decodeBlp
  , decodeBlpMipmaps
  , writeBlpJpeg
  , writeBlpUncompressedWithAlpha
  , writeBlpUncompressedWithoutAlpha
  , encodeBlpJpeg
  , encodeBlpUncompressedWithAlpha
  , encodeBlpUncompressedWithoutAlpha
  , mipMapsUpTo
  ) where

import Codec.Picture
import Data.Bits (shiftR)
import Data.ByteString (ByteString)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as V

import Codec.Picture.Blp.Internal.Convert
import Codec.Picture.Blp.Internal.Data
import Codec.Picture.Blp.Internal.Encoder
import Codec.Picture.Blp.Internal.Parser

-- | Read BLP from given file without mipmaps
readBlp :: FilePath -> IO (Either String DynamicImage)
readBlp fp = decodeBlp `fmap` BS.readFile fp

-- | Read BLP from given file with mipmaps
readBlpMipmaps :: FilePath -> IO (Either String [DynamicImage])
readBlpMipmaps fp = decodeBlpMipmaps `fmap` BS.readFile fp

-- | Decodes BLP without mipmaps
decodeBlp :: ByteString -> Either String DynamicImage
decodeBlp bs = do
  is <- decodeBlpMipmaps bs
  case is of
    i : _ -> Right i
    [] -> Left "No data in BLP"

-- | Decodes BLP and returns original image plus all mipmaps
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
              useAlpha = BlpFlagAlphaChannel `elem` blpFlags blp && jpegAlphaIsUsable mip0
          Right $ fromBlpJpeg useAlpha <$> (mip0 : lazyTail)

    BlpUncompressed1 {..} -> uncompressedLevels width height fits render blpU1MipMaps
      where
      fits n (indexList, alphaList) = BS.length indexList >= n && BS.length alphaList >= n
      render w (indexList, alphaList) x y = let
        i = y * w + x
        PixelRGBA8 r g b _ = blpU1Palette V.! fromIntegral (indexList `BS.index` i)
        in PixelRGBA8 b g r (alphaList `BS.index` i)

    BlpUncompressed2 {..} -> uncompressedLevels width height fits render blpU2MipMaps
      where
      fits n indexList = BS.length indexList >= n
      render w indexList x y = let
        PixelRGBA8 r g b _ = blpU2Palette V.! fromIntegral (indexList `BS.index` (y * w + x))
        -- Without-alpha textures have no alpha to decode; the palette's fourth
        -- byte is not alpha and tools fill it inconsistently.
        in PixelRGBA8 b g r 255

-- | Render the mipmaps of an uncompressed BLP. Level @l@ of a @w@ x @h@
-- texture is @w / 2^l@ by @h / 2^l@, but never smaller than 1x1. The list
-- stops at the first level whose buffer is too short for its size, so the
-- result is always a prefix of the mip levels; a short level 0 fails the file.
uncompressedLevels :: Int -> Int
  -> (Int -> mip -> Bool) -- ^ Does the buffer hold this many pixels?
  -> (Int -> mip -> Int -> Int -> PixelRGBA8) -- ^ Pixel at (x, y) of a level of given width
  -> [mip]
  -> Either String [DynamicImage]
uncompressedLevels width height fits render mips
  | null mips = Right []
  | otherwise = case go 0 mips of
      [] -> Left "Mipmap 0 is too short for the image size"
      levels -> Right levels
  where
    go l (mip : rest)
      | fits (w * h) mip = ImageRGBA8 (generateImage (render w mip) w h) : go (l + 1) rest
      where
        w = levelSide width l
        h = levelSide height l
    go _ _ = []
    levelSide side l = if side <= 0 then 0 else max 1 (side `shiftR` l)

-- | Calculate needed count of mipmaps to cover sizes up to given minimum size (helper for 'writeBlp*' functions)
mipMapsUpTo :: Int -- ^ Minimum size of picture side for generation of mipmap (ex. 2, 4, 16, 32, 64, 512, 1024 and etc)
  -> DynamicImage -- ^ Image for which we generate mipmaps
  -> Int
-- Counts the levels whose smaller side is at least the minimum (mip 0
-- always counts), matching where the encoder's mip chain stops.
mipMapsUpTo minSize img = max 1 . length . takeWhile (>= max 1 minSize) $ iterate (`div` 2) minSide
  where
    minSide = min (dynamicMap imageWidth img) (dynamicMap imageHeight img)

writeBlpJpeg :: FilePath -> Int -> Int -> DynamicImage -> IO ()
writeBlpJpeg fp quality numMips img = do
  let bs = encodeBlpJpeg quality numMips img
  BSL.writeFile fp bs

writeBlpUncompressedWithAlpha :: FilePath -> Int -> DynamicImage -> IO ()
writeBlpUncompressedWithAlpha fp numMips img = do
  let bs = encodeBlpUncompressedWithAlpha numMips img
  BSL.writeFile fp bs

writeBlpUncompressedWithoutAlpha :: FilePath -> Int -> DynamicImage -> IO ()
writeBlpUncompressedWithoutAlpha fp numMips img = do
  let bs = encodeBlpUncompressedWithoutAlpha numMips img
  BSL.writeFile fp bs

encodeBlpJpeg :: Int -> Int -> DynamicImage -> BSL.ByteString
encodeBlpJpeg quality numMips = encodeBlp numMips . toBlpStruct BlpCompressionJPEG quality numMips

encodeBlpUncompressedWithAlpha :: Int -> DynamicImage -> BSL.ByteString
encodeBlpUncompressedWithAlpha numMips = encodeBlp numMips . toBlpStruct BlpCompressionUncompressed 100 numMips

encodeBlpUncompressedWithoutAlpha :: Int -> DynamicImage -> BSL.ByteString
encodeBlpUncompressedWithoutAlpha numMips = encodeBlp numMips . toBlpStruct BlpCompressionUncompressed 100 numMips
