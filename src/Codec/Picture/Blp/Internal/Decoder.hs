{-|
Turning a parsed BLP1 file into images: the size limit, JPEG levels with
their shared header and alpha decision, and paletted levels with packed
alpha. "Codec.Picture.Blp" parses the file and calls 'decodeBlpStruct'.

This is an internal module: its API may change between minor versions.
-}
module Codec.Picture.Blp.Internal.Decoder(
    decodeBlpStruct
  , blpStructInfo
  ) where

import Codec.Picture
import Control.DeepSeq (force)
import Control.Exception
import Control.Monad (when)
import Data.Bits ((.&.), shiftR)
import Data.ByteString (ByteString)
import System.IO.Unsafe (unsafeDupablePerformIO)

import qualified Data.ByteString as BS
import qualified Data.Vector as V

import Codec.Picture.Blp.Internal.Convert
import Codec.Picture.Blp.Internal.Data
import Codec.Picture.Blp.Internal.Parser
import Codec.Picture.Blp.Types

-- | Decode the full size image of a parsed file followed by its mipmaps, all
-- 'ImageRGBA8'. See 'Codec.Picture.Blp.decodeBlpMipmapsWith'.
decodeBlpStruct :: BlpDecodeOptions -> BlpStruct -> Either String [DynamicImage]
decodeBlpStruct opts blp = do
  let width = fromIntegral $ blpWidth blp
      height = fromIntegral $ blpHeight blp
  when (width * height > blpMaxPixels opts) . Left $
    "Image size " ++ show width ++ "x" ++ show height ++ " is above the limit of "
    ++ show (blpMaxPixels opts) ++ " pixels"
  case blpExt blp of
    BlpJpeg {..} -> case blpJpegData of
      [] -> Left "No JPEG mipmap data in BLP"
      level0 : rest -> do
        -- Mipmap 0 is the full-resolution image; a failure there fails the
        -- file. Tail mipmaps are decoded lazily and the chain stops at the
        -- first one that fails, so a corrupt tail costs only the levels from
        -- it on, and element i stays level i.
        mip0 <- decodeLevel 0 level0
        let validPrefix l (j : js) | Right m <- decodeLevel l j = m : validPrefix (l + 1) js
            validPrefix _ _ = []
            -- The alpha decision is taken once, from the full size mipmap, so that
            -- every mipmap of a texture is decoded the same way. Smaller mipmaps
            -- are often padded out with a flat alpha plane of their own.
            useAlpha = blpAlphaBits blp == 8 && jpegAlphaIsUsable mip0
        Right $ toLevel useAlpha <$> zip [0 ..] (mip0 : validPrefix 1 rest)
      where
      -- The frame size is checked before decoding: JuicyPixels allocates the
      -- whole image up front, so a forged frame header could otherwise ask
      -- for gigabytes. A frame larger than the level is rejected; a smaller
      -- one is padded with transparent black, as Warcraft III does.
      decodeLevel :: Int -> ByteString -> Either String DynamicImage
      decodeLevel l level = do
        let jpeg = blpJpegHeader <> level
            (lw, lh) = levelSize width height l
        (fw, fh) <- maybe (Left $ "JPEG mipmap " ++ show l ++ " has no frame header") Right (jpegFrameSize jpeg)
        if fw > lw || fh > lh
          then Left $ "JPEG mipmap " ++ show l ++ " is " ++ show fw ++ "x" ++ show fh
            ++ ", larger than its level size " ++ show lw ++ "x" ++ show lh
          else safeDecodeJpeg jpeg
      toLevel useAlpha (l, img) = let
        (lw, lh) = levelSize width height l
        rgba = convertRGBA8 (fromBlpJpeg useAlpha img)
        in ImageRGBA8 $ if imageWidth rgba == lw && imageHeight rgba == lh
          then rgba
          else generateImage (\x y -> if x < imageWidth rgba && y < imageHeight rgba
            then pixelAt rgba x y else PixelRGBA8 0 0 0 0) lw lh

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

-- | Size of mip level @l@ of a @w@ x @h@ texture: each side halved @l@ times,
-- but never below one pixel.
levelSize :: Int -> Int -> Int -> (Int, Int)
levelSize w h l = (max 1 (w `shiftR` l), max 1 (h `shiftR` l))

-- | 'decodeJpeg', with errors JuicyPixels raises from pure code turned into
-- 'Left'. Asynchronous exceptions are rethrown.
safeDecodeJpeg :: ByteString -> Either String DynamicImage
safeDecodeJpeg jpeg = unsafeDupablePerformIO $ do
  result <- try (evaluate (force (decodeJpeg jpeg)))
  case result of
    Right decoded -> return decoded
    Left e
      | Just async <- fromException e -> throwIO (async :: SomeAsyncException)
      | otherwise -> return . Left $ "JPEG decoding failed: " ++ displayException (e :: SomeException)


-- | What 'Codec.Picture.Blp.blpInfo' reports for a parsed file.
blpStructInfo :: BlpStruct -> BlpInfo
blpStructInfo blp = BlpInfo {
    blpInfoCompression = blpCompression blp
  , blpInfoWidth = fromIntegral $ blpWidth blp
  , blpInfoHeight = fromIntegral $ blpHeight blp
  , blpInfoAlphaBits = fromIntegral $ blpAlphaBits blp
  , blpInfoMipLevels = case blpExt blp of
      BlpJpeg {..} -> length blpJpegData
      BlpPaletted {..} -> length blpPalettedMipMaps
  }

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
        (w, h) = levelSize width height l
    go _ _ = []

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

