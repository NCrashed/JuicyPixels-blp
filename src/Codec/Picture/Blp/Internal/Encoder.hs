{-|
Building and serialising BLP1 files: palette quantisation, the mip chain, the
shared JPEG header and the binary layout.

This is an internal module: its API may change between minor versions.
-}
module Codec.Picture.Blp.Internal.Encoder(
    encodeBlp
  , blpEncoder
  , createMipMaps
  , createMipMapsIndexed
  , scanHeader
  , toBlpStruct
  , toBlpExt
  ) where

import Codec.Picture
import Codec.Picture.ColorQuant
import Codec.Picture.Jpg
import Codec.Picture.Types
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.Foldable (traverse_)
import Data.Word

import qualified Codec.Picture.Metadata as CM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI
import qualified Data.Foldable as F
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

import Codec.Picture.Blp.Internal.Convert
import Codec.Picture.Blp.Internal.Data

-- | Serialise a BLP structure, writing the given number of mip levels (at
-- least one).
encodeBlp :: Int -> BlpStruct -> ByteString
encodeBlp numMips = runPut . blpEncoder numMips

-- | Raw encoder for BLP
blpEncoder :: Int -> BlpStruct -> Put
blpEncoder requestedMips BlpStruct{..} = do
  putBlpVersion
  putCompression
  putWord32le blpAlphaBits
  putWord32le blpWidth
  putWord32le blpHeight
  putWord32le blpExtra
  putWord32le blpHasMipmaps
  putMipMapOffsets
  putMipMapSizes
  putBlpExt
  where
    -- A file without mip 0 has no image at all, so always write it.
    numMips = max 1 requestedMips
    putBlpVersion = putByteString "BLP1"
    putCompression = putWord32le $ case blpCompression of
      BlpCompressionJPEG -> 0
      BlpCompressionUncompressed -> 1
    headerSize = 4 -- BLP1
      + 4 -- Compression
      + 4 -- Alpha bits
      + 4 -- Width
      + 4 -- Height
      + 4 -- Extra
      + 4 -- Has mipmaps
      + 16 * 4 -- Mipmaps offsets
      + 16 * 4 -- Mipmaps sizes
    mipmaps = let
      mkOffsetSize :: BS.ByteString -> (Int, [(Word32, Word32)]) ->  (Int, [(Word32, Word32)])
      mkOffsetSize bs (!offset, !acc) = (offset + BS.length bs, (fromIntegral offset, fromIntegral $ BS.length bs) : acc)
      uncomprStartOffset = headerSize + 4 * 256
      in reverse . snd $ case blpExt of
        BlpJpeg{..} -> let
          startOffset = headerSize + 4 + BS.length blpJpegHeader
          in F.foldr' mkOffsetSize (startOffset, []) $ reverse blpJpegData
        BlpPaletted{..} -> F.foldr' mkOffsetSize (uncomprStartOffset, []) $ reverse blpPalettedMipMaps
    ensureLength _ [] = []
    ensureLength n xs = if length xs < n
      then xs ++ replicate (n - length xs) (last xs)
      else take n xs
    ensureLengthV n a xs = if V.length xs < n then xs <> V.replicate (n - V.length xs) a else V.take n xs
    putMipMapOffsets = traverse_ putWord32le . ensureLength 16 . take numMips . fmap fst $ mipmaps
    putMipMapSizes = traverse_ putWord32le . ensureLength 16 . take numMips . fmap snd $ mipmaps
    putRgba8 (PixelRGBA8 r g b a) = putWord8 r >> putWord8 g >> putWord8 b >> putWord8 a
    putBlpExt = case blpExt of -- TODO: check sync of compression flag and BlpExt
       BlpJpeg{..} -> do
         putWord32le $ fromIntegral $ BS.length blpJpegHeader
         putByteString blpJpegHeader
         traverse_ putByteString $ take numMips blpJpegData
       BlpPaletted{..} -> do
         traverse_ putRgba8 . ensureLengthV 256 (PixelRGBA8 0 0 0 0) $ blpPalette
         traverse_ putByteString $ take numMips blpPalettedMipMaps

-- | Return 'True' if given picture format has alpha channel
hasAlpha :: DynamicImage -> Bool
hasAlpha img = case img of
  ImageYA8 _ -> True
  ImageYA16 _ -> True
  ImageRGBA8 _ -> True
  ImageRGBA16 _ -> True
  _ -> False

-- | Build the BLP structure for an image: content type, JPEG quality (ignored
-- for paletted content) and number of mip levels. Alpha follows the image
-- format.
toBlpStruct :: BlpCompression -> Int -> Int -> DynamicImage -> BlpStruct
toBlpStruct compression quality numMips img = BlpStruct {
    blpCompression = compression
  , blpAlphaBits = if alpha then 8 else 0
  , blpWidth = fromIntegral $ dynamicMap imageWidth img
  , blpHeight = fromIntegral $ dynamicMap imageHeight img
  -- Readers that follow the old description of the format pick alpha by
  -- this field (3 or 4 with alpha, 5 without), so keep it consistent.
  , blpExtra = if alpha then 3 else 5
  , blpHasMipmaps = 5 -- WorldEdit writes this for war3mapMap.blp
  , blpExt = toBlpExt compression alpha quality numMips img'
  }
  where
    alpha = hasAlpha img
    img' = convertRGBA8 img

-- | Take only first N mipmaps in list, fill rest with last non-fake mipmap
fakeMipMaps :: Int -- ^ How much true values to preserve (if <= 0, the function does nothing)
  -> [a] -- ^ List of mimpaps
  -> [a] -- ^ Result with fakes
fakeMipMaps = go Nothing
  where
    go mprev n xs
      | n <= 0 = case mprev of
        Nothing -> xs
        Just v -> case xs of
          [] -> []
          _ : xs' -> v : go (Just v) 0 xs'
      | otherwise = case xs of
        [] -> []
        x : xs' -> x : go (Just x) (n-1) xs'

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

-- | Convert picture to BLP payload
toBlpExt :: BlpCompression -> Bool -> Int -> Int -> Image PixelRGBA8 -> BlpExt
toBlpExt compr keepAlpha quality numMips img = case compr of
  BlpCompressionJPEG -> toBlpJpg (fromIntegral quality) numMips keepAlpha img
  BlpCompressionUncompressed -> toBlpPaletted keepAlpha numMips img

-- | Convert picture to BLP JPEG and create mipmaps
toBlpJpg :: Word8 -> Int -> Bool -> Image PixelRGBA8 -> BlpExt
toBlpJpg quality numMips keepAlpha img = BlpJpeg {
    blpJpegHeader = header
  , blpJpegData = mipmapsRawWithoutHeader
  }
  where
    processAlpha :: Image PixelRGBA8 -> Image PixelRGBA8
    processAlpha = pixelMap $ \p@(PixelRGBA8 r g b _) -> if keepAlpha then p else PixelRGBA8 r g b 255

    mipmaps :: [Image PixelCMYK8]
    mipmaps = toBlpCMYK8 <$> (fakeMipMaps numMips . createMipMaps . processAlpha $ img)

    metadata :: CM.Metadatas
    metadata = CM.insert (CM.Unknown "JPEG Quality") (CM.Int $ fromIntegral quality) mempty

    mipmapsRaw :: [BS.ByteString]
    mipmapsRaw = toStrict . encodeDirectJpegAtQualityWithMetadata quality metadata <$> mipmaps

    header :: BS.ByteString
    header = scanHeader 624 mipmapsRaw

    mipmapsRawWithoutHeader :: [BS.ByteString]
    mipmapsRawWithoutHeader = BS.drop (BS.length header) <$> mipmapsRaw

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
toBlpPaletted keepAlpha numMips img = BlpPaletted {
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
    mipmaps = fakeMipMaps numMips $ createMipMapsIndexed img'

    alphaMaps :: [Image Pixel8]
    alphaMaps = fakeMipMaps numMips $ createMipMapsIndexed (extractComponent PlaneAlpha img)
