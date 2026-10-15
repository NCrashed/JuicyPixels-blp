{-|
Building and serialising BLP1 files: palette quantisation, the mip chain, the
shared JPEG header and the binary layout.

This is an internal module: its API may change between minor versions.
-}
module Codec.Picture.Blp.Internal.Encoder(
    encodeBlp
  , blpEncoder
  , toBlpStruct
  , createMipMaps
  , createMipMapsIndexed
  , scanHeader
  ) where

import Codec.Picture
import Codec.Picture.ColorQuant
import Codec.Picture.Jpg
import Codec.Picture.Types
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.Foldable (traverse_)
import Data.Word

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

import Codec.Picture.Blp.Internal.Convert
import Codec.Picture.Blp.Internal.Data

-- | Serialise a BLP structure. The content type follows from 'blpExt'; at
-- most 16 mip levels are written, and unused slots are zero.
encodeBlp :: BlpStruct -> ByteString
encodeBlp = runPut . blpEncoder

-- | Raw encoder for BLP
blpEncoder :: BlpStruct -> Put
blpEncoder BlpStruct{..} = do
  putByteString "BLP1"
  putWord32le $ case blpExt of
    BlpJpeg {} -> 0
    BlpPaletted {} -> 1
  putWord32le blpAlphaBits
  putWord32le blpWidth
  putWord32le blpHeight
  putWord32le blpExtra
  putWord32le blpHasMipmaps
  traverse_ putWord32le . slots $ take (length levels) (scanl (+) contentStart levelSizes)
  traverse_ putWord32le $ slots levelSizes
  case blpExt of
    BlpJpeg {..} -> do
      putWord32le . fromIntegral $ BS.length blpJpegHeader
      putByteString blpJpegHeader
    BlpPaletted {..} ->
      traverse_ putRgba8 . V.take 256 $ blpPalette <> V.replicate 256 (PixelRGBA8 0 0 0 0)
  traverse_ putByteString levels
  where
    headerSize = 156
    levels = take 16 $ case blpExt of
      BlpJpeg {..} -> blpJpegData
      BlpPaletted {..} -> blpPalettedMipMaps
    levelSizes = BS.length <$> levels
    contentStart = case blpExt of
      BlpJpeg {..} -> headerSize + 4 + BS.length blpJpegHeader
      BlpPaletted {} -> headerSize + 4 * 256
    slots xs = take 16 $ (fromIntegral <$> xs) ++ repeat 0
    putRgba8 (PixelRGBA8 r g b a) = putWord8 r >> putWord8 g >> putWord8 b >> putWord8 a

-- | Return 'True' if given picture format has alpha channel
hasAlpha :: DynamicImage -> Bool
hasAlpha img = case img of
  ImageYA8 _ -> True
  ImageYA16 _ -> True
  ImageRGBA8 _ -> True
  ImageRGBA16 _ -> True
  _ -> False

-- | Build the BLP structure for an image with the given format and number of
-- mip levels. The number is clamped to between 1 and the image's full chain,
-- at most 16. The image's size is not checked.
toBlpStruct :: BlpFormat -> Int -> DynamicImage -> BlpStruct
toBlpStruct format requestedLevels img = BlpStruct {
    blpAlphaBits = if alpha then 8 else 0
  , blpWidth = fromIntegral $ imageWidth rgba
  , blpHeight = fromIntegral $ imageHeight rgba
  -- Readers that follow the old description of the format pick alpha by
  -- this field (3 or 4 with alpha, 5 without), so keep it consistent.
  , blpExtra = if alpha then 3 else 5
  -- WorldEdit writes 5; a single level says it has no mipmaps.
  , blpHasMipmaps = if numLevels > 1 then 5 else 0
  , blpExt = case format of
      BlpFormatJpeg quality -> toBlpJpg (fromIntegral quality) numLevels alpha rgba
      BlpFormatPaletted _ -> toBlpPaletted alpha numLevels rgba
  }
  where
    alpha = case format of
      BlpFormatJpeg _ -> hasAlpha img
      BlpFormatPaletted withAlpha -> withAlpha
    rgba = convertRGBA8 img
    chainLength = length . takeWhile (>= 1) $ iterate (`div` 2) (max (imageWidth rgba) (imageHeight rgba))
    numLevels = max 1 . min 16 $ min chainLength requestedLevels

-- | Scale image to form the sequence of mipmaps. The first element is always the original picture.
--
-- Each level averages 2x2 blocks; on an odd or one-pixel side the last block
-- reuses the edge pixel.
createMipMaps :: Image PixelRGBA8 -> [Image PixelRGBA8]
createMipMaps img = img : go img
  where
    avg' v1 v2 v3 v4 = let
      v = fromIntegral v1 + fromIntegral v2 + fromIntegral v3 + fromIntegral v4 :: Double
      in round $ v / 4
    -- On a side that is already one pixel (or odd) the second sample is
    -- clamped to the edge, so the chain can go on to 1x1.
    avg i x y = let
      x1 = min (imageWidth i - 1) (x*2 + 1)
      y1 = min (imageHeight i - 1) (y*2 + 1)
      PixelRGBA8 p00r p00g p00b p00a = pixelAt i (x*2) (y*2)
      PixelRGBA8 p10r p10g p10b p10a = pixelAt i x1 (y*2)
      PixelRGBA8 p01r p01g p01b p01a = pixelAt i (x*2) y1
      PixelRGBA8 p11r p11g p11b p11a = pixelAt i x1 y1
      in PixelRGBA8 (avg' p00r p10r p01r p11r) (avg' p00g p10g p01g p11g) (avg' p00b p10b p01b p11b) (avg' p00a p10a p01a p11a)
    power2Scale i = generateImage (avg i) (downgrade $ imageWidth i) (downgrade $ imageHeight i)
    downgrade v = max 1 $ v `div` 2
    go i | imageWidth i <= 1 && imageHeight i <= 1 = []
         | otherwise = let i' = power2Scale i in i' : go i'

-- | Scale image to form the sequence of mipmaps, down to 1x1. The first
-- element is always the original picture. Each level takes the top-left pixel
-- of every 2x2 block, as averaging palette indices makes no sense.
createMipMapsIndexed :: Pixel a => Image a -> [Image a]
createMipMapsIndexed img = img : go img
  where
    power2Scale i = generateImage (\x y -> pixelAt i (x*2) (y*2)) (downgrade $ imageWidth i) (downgrade $ imageHeight i)
    downgrade v = max 1 $ v `div` 2
    go i | imageWidth i <= 1 && imageHeight i <= 1 = []
         | otherwise = let i' = power2Scale i in i' : go i'

-- | Convert picture to BLP JPEG and create mipmaps
toBlpJpg :: Word8 -> Int -> Bool -> Image PixelRGBA8 -> BlpExt
toBlpJpg quality numLevels keepAlpha img = BlpJpeg {
    blpJpegHeader = header
  , blpJpegData = BS.drop (BS.length header) <$> streams
  }
  where
    opaque = if keepAlpha then id else pixelMap (\(PixelRGBA8 r g b _) -> PixelRGBA8 r g b 255)
    levels = take numLevels . createMipMaps . opaque $ img
    streams = toStrict . encodeDirectJpegAtQualityWithMetadata quality mempty . toBlpCMYK8 <$> levels
    header = scanHeader 624 streams

-- | Manually scan shared prefix of each mipmap
scanHeader :: Int -> [BS.ByteString] -> BS.ByteString
scanHeader _ [] = mempty
scanHeader maxheader mipmaps@(m0 : _) = BS.take common m0
  where
    -- Each mipmap keeps at least one byte of its own: faked mipmaps are
    -- copies of a real one, and a header swallowing a whole stream would
    -- leave a zero-size slot that readers skip.
    limit = min maxheader (minimum (BS.length <$> mipmaps) - 1)
    common = length . takeWhile sameAt $ [0 .. limit - 1]
    -- Safe to index: every stream is longer than the limit.
    sameAt i = all (\m -> BS.index m i == BS.index m0 i) mipmaps

-- | Helper to quantise colors to 256 colour pallete
makePallette :: Image PixelRGBA8 -> (Image Pixel8, Palette)
makePallette = palettize defaultPaletteOptions . pixelMap dropTransparency

-- | Convert palette to format that we need for BLP
convertPalette :: Palette -> V.Vector PixelRGBA8
convertPalette palette = V.generate (imageWidth palette) $ \i ->
  let PixelRGB8 r g b = pixelAt palette i 0 in PixelRGBA8 b g r 255

-- | Convert indexed image to raw bytestring
convertIndexed :: Image Pixel8 -> BS.ByteString
convertIndexed img = let
  (fptr, l) = VS.unsafeToForeignPtr0 . imageData $ img
  -- Shares the image buffer without copying; the ByteString keeps it alive.
  in BI.fromForeignPtr fptr 0 l

-- | Quantise a picture to a 256 colour palette and create mipmaps, with an
-- 8-bit alpha list after each index list when asked to keep alpha.
toBlpPaletted :: Bool -> Int -> Image PixelRGBA8 -> BlpExt
toBlpPaletted keepAlpha numLevels img = BlpPaletted {
    blpPalette = convertPalette palette
  , blpPalettedMipMaps = if keepAlpha
      then zipWith (<>) (convertIndexed <$> mipmaps) (convertIndexed <$> alphaMaps)
      else convertIndexed <$> mipmaps
  }
  where
    img'    :: Image Pixel8
    palette :: Palette
    (img', palette) = makePallette img

    mipmaps :: [Image Pixel8]
    mipmaps = take numLevels $ createMipMapsIndexed img'

    alphaMaps :: [Image Pixel8]
    alphaMaps = take numLevels $ createMipMapsIndexed (extractComponent PlaneAlpha img)
