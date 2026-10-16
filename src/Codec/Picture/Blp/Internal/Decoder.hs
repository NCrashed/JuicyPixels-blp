{-|
Turning a parsed BLP1 or BLP2 file into images: the size limit, JPEG levels
with their shared header and alpha decision, paletted levels with packed
alpha, and BLP2's DXT and BGRA levels. "Codec.Picture.Blp" parses the file
and calls 'decodeBlpFile'.

This is an internal module: its API may change between minor versions.
-}
module Codec.Picture.Blp.Internal.Decoder(
    decodeBlpFile
  , blpFileInfo
  , decodeBlpStruct
  , decodeBlp2Struct
  , blpStructInfo
  , blp2StructInfo
  ) where

import Codec.Picture
import Control.DeepSeq (force)
import Control.Exception
import Control.Monad (when)
import Data.Bits ((.&.), shiftR)
import Data.ByteString (ByteString)
import Data.Either (isRight)
import System.IO.Unsafe (unsafeDupablePerformIO)

import qualified Data.ByteString as BS
import qualified Data.Vector as V

import Codec.Picture.Blp.Internal.Convert
import Codec.Picture.Blp.Internal.Data
import Codec.Picture.Blp.Internal.Dxt
import Codec.Picture.Blp.Internal.Parser
import Codec.Picture.Blp.Types

-- | Decode the full size image of a parsed file followed by its mipmaps, all
-- 'ImageRGBA8'. See 'Codec.Picture.Blp.decodeBlpMipmapsWith'.
decodeBlpStruct :: BlpDecodeOptions -> BlpStruct -> Either String [DynamicImage]
decodeBlpStruct opts blp = do
  let width = fromIntegral $ blpWidth blp
      height = fromIntegral $ blpHeight blp
  checkSize opts width height
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

    BlpPaletted {..} -> palettedImages width height (fromIntegral (blpAlphaBits blp)) blpPalette blpPalettedMipMaps

-- | Decode the full size image of a parsed BLP2 file followed by its
-- mipmaps, all 'ImageRGBA8'. Levels are taken as in BLP1: a damaged level 0
-- fails the file, and the list stops at the first damaged mipmap. An alpha
-- depth of 0 makes every level opaque.
decodeBlp2Struct :: BlpDecodeOptions -> Blp2Struct -> Either String [DynamicImage]
decodeBlp2Struct opts blp = do
  checkSize opts width height
  case blp2Content blp of
    Blp2Paletted palette -> palettedImages width height (fromIntegral (blp2AlphaDepth blp)) palette (blp2MipMaps blp)
    Blp2Dxt fmt -> levels (\(lw, lh) -> decodeDxt fmt lw lh)
    Blp2Bgra -> levels bgraLevel
  where
    width = fromIntegral $ blp2Width blp
    height = fromIntegral $ blp2Height blp
    finish = if blp2AlphaDepth blp == 0 then setOpaque else id
    levels decode = case blp2MipMaps blp of
      [] -> Left "No mipmap data in BLP2"
      mips -> case [decode (levelSize width height l) mip | (l, mip) <- zip [0 ..] mips] of
        Left e : _ -> Left $ "Mipmap 0: " ++ e
        decoded -> Right [ImageRGBA8 (finish img) | Right img <- takeWhile isRight decoded]

-- | A level of four bytes per pixel, B, G, R and A.
bgraLevel :: (Int, Int) -> ByteString -> Either String (Image PixelRGBA8)
bgraLevel (w, h) bytes
  | BS.length bytes < 4 * w * h = Left $ "BGRA data is " ++ show (BS.length bytes)
      ++ " bytes, a " ++ show w ++ "x" ++ show h ++ " level needs " ++ show (4 * w * h)
  | otherwise = Right $ generateImage pixel w h
  where
    pixel x y = let p = 4 * (y * w + x); b k = BS.index bytes (p + k)
                in PixelRGBA8 (b 2) (b 1) (b 0) (b 3)

setOpaque :: Image PixelRGBA8 -> Image PixelRGBA8
setOpaque = pixelMap (\(PixelRGBA8 r g b _) -> PixelRGBA8 r g b 255)

-- | Reject an image above the size limit before anything is allocated.
checkSize :: BlpDecodeOptions -> Int -> Int -> Either String ()
checkSize opts width height = when (width * height > blpMaxPixels opts) . Left $
  "Image size " ++ show width ++ "x" ++ show height ++ " is above the limit of "
  ++ show (blpMaxPixels opts) ++ " pixels"

-- | Paletted levels with the declared alpha depth, if it is valid for
-- paletted content. A mip 0 too short to hold that much alpha has none:
-- some tools declare alpha and then write only the index list.
palettedImages :: Int -> Int -> Int -> V.Vector PixelRGBA8 -> [ByteString] -> Either String [DynamicImage]
palettedImages width height declaredBits palette mips = palettedLevels width height alphaBits palette mips
  where
    declared = if declaredBits `elem` [1, 4, 8] then declaredBits else 0
    alphaBits = case mips of
      mip0 : _ | BS.length mip0 < levelBytes (width * height) declared -> 0
      _ -> declared

-- | Decode a parsed BLP1 or BLP2 file.
decodeBlpFile :: BlpDecodeOptions -> BlpFile -> Either String [DynamicImage]
decodeBlpFile opts (BlpFile1 blp) = decodeBlpStruct opts blp
decodeBlpFile opts (BlpFile2 blp) = decodeBlp2Struct opts blp

-- | What 'Codec.Picture.Blp.blpInfo' reports for a parsed BLP1 or BLP2 file.
blpFileInfo :: BlpFile -> BlpInfo
blpFileInfo (BlpFile1 blp) = blpStructInfo blp
blpFileInfo (BlpFile2 blp) = blp2StructInfo blp

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
    blpInfoVersion = Blp1
  , blpInfoCompression = blpCompression blp
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


-- | What 'Codec.Picture.Blp.blpInfo' reports for a parsed BLP2 file.
blp2StructInfo :: Blp2Struct -> BlpInfo
blp2StructInfo blp = BlpInfo {
    blpInfoVersion = Blp2
  , blpInfoCompression = blp2Compression blp
  , blpInfoWidth = fromIntegral $ blp2Width blp
  , blpInfoHeight = fromIntegral $ blp2Height blp
  , blpInfoAlphaBits = fromIntegral $ blp2AlphaDepth blp
  , blpInfoMipLevels = length (blp2MipMaps blp)
  }
