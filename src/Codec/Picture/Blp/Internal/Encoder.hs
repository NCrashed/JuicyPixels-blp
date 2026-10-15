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
import qualified Data.Vector.Storable.Mutable as MVS

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
  , blpWidth = fromIntegral width
  , blpHeight = fromIntegral height
  -- Readers that follow the old description of the format pick alpha by
  -- this field (3 or 4 with alpha, 5 without), so keep it consistent.
  , blpExtra = if alpha then 3 else 5
  -- WorldEdit writes 5; a single level says it has no mipmaps.
  , blpHasMipmaps = if numLevels > 1 then 5 else 0
  , blpExt = case format of
      BlpFormatJpeg quality -> toBlpJpg (fromIntegral quality) numLevels alpha rgba
      BlpFormatPaletted _ -> toBlpPaletted alpha numLevels (convertRGB8 img) rgba
  }
  where
    alpha = case format of
      BlpFormatJpeg _ -> hasAlpha img
      BlpFormatPaletted withAlpha -> withAlpha
    -- Only built when needed: paletted textures without alpha go from RGB.
    rgba = convertRGBA8 img
    width = dynamicMap imageWidth img
    height = dynamicMap imageHeight img
    chainLength = length . takeWhile (>= 1) $ iterate (`div` 2) (max width height)
    numLevels = max 1 . min 16 $ min chainLength requestedLevels

-- | Scale image to form the sequence of mipmaps. The first element is always the original picture.
--
-- Each level averages 2x2 blocks; on an odd or one-pixel side the last block
-- reuses the edge pixel. Colour is weighted by alpha, so the colour of
-- transparent pixels does not bleed into their neighbours; alpha itself is
-- a plain average. Rounding is to the nearest, ties to even.
createMipMaps :: Image PixelRGBA8 -> [Image PixelRGBA8]
createMipMaps img = img : go img
  where
    go i | imageWidth i <= 1 && imageHeight i <= 1 = []
         | otherwise = let i' = halveRGBA8 i in i' : go i'

-- | One step of 'createMipMaps': halve both sides, but not below one pixel.
-- On a side that is already one pixel (or odd) the second sample of a block
-- is clamped to the edge.
halveRGBA8 :: Image PixelRGBA8 -> Image PixelRGBA8
halveRGBA8 (Image w h src) = Image w' h' $ VS.create $ do
  dst <- MVS.new (w' * h' * 4)
  let pixel x y = do
        let x0 = 2 * x
            x1 = min (w - 1) (x0 + 1)
            y0 = 2 * y
            y1 = min (h - 1) (y0 + 1)
            o00 = (y0 * w + x0) * 4
            o10 = (y0 * w + x1) * 4
            o01 = (y1 * w + x0) * 4
            o11 = (y1 * w + x1) * 4
            at o = fromIntegral (VS.unsafeIndex src o) :: Int
            a00 = at (o00 + 3)
            a10 = at (o10 + 3)
            a01 = at (o01 + 3)
            a11 = at (o11 + 3)
            alphaSum = a00 + a10 + a01 + a11
            -- Colour weighted by alpha, or a plain average if all four are
            -- transparent.
            channel c
              | alphaSum == 0 = roundDiv (at (o00 + c) + at (o10 + c) + at (o01 + c) + at (o11 + c)) 4
              | otherwise = roundDiv (at (o00 + c) * a00 + at (o10 + c) * a10 + at (o01 + c) * a01 + at (o11 + c) * a11) alphaSum
            out = (y * w' + x) * 4
        MVS.unsafeWrite dst out (channel 0)
        MVS.unsafeWrite dst (out + 1) (channel 1)
        MVS.unsafeWrite dst (out + 2) (channel 2)
        MVS.unsafeWrite dst (out + 3) (roundDiv alphaSum 4)
      loop y x
        | y >= h' = return ()
        | x >= w' = loop (y + 1) 0
        | otherwise = pixel x y >> loop y (x + 1)
  loop 0 0
  return dst
  where
    w' = max 1 (w `div` 2)
    h' = max 1 (h `div` 2)
    -- Nearest, ties to even.
    roundDiv :: Int -> Int -> Word8
    roundDiv n d = let
      (q, r) = n `divMod` d
      in fromIntegral $ if 2 * r > d || (2 * r == d && odd q) then q + 1 else q

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
    opaque = if keepAlpha then id else opaqueRGBA8
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
makePallette :: Image PixelRGB8 -> (Image Pixel8, Palette)
makePallette = palettize defaultPaletteOptions

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
toBlpPaletted :: Bool -> Int -> Image PixelRGB8 -> Image PixelRGBA8 -> BlpExt
toBlpPaletted keepAlpha numLevels rgb rgba = BlpPaletted {
    blpPalette = convertPalette palette
  , blpPalettedMipMaps = if keepAlpha
      then zipWith (<>) (convertIndexed <$> mipmaps) (convertIndexed <$> alphaMaps)
      else convertIndexed <$> mipmaps
  }
  where
    img'    :: Image Pixel8
    palette :: Palette
    (img', palette) = makePallette rgb

    mipmaps :: [Image Pixel8]
    mipmaps = take numLevels $ createMipMapsIndexed img'

    -- Alpha is coverage, not an index, so its mipmaps are averaged.
    alphaMaps :: [Image Pixel8]
    alphaMaps = take numLevels $ extractComponent PlaneAlpha <$> createMipMaps rgba
