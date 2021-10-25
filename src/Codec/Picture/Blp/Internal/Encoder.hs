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
import Data.Bits
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.Foldable (traverse_)
import Data.Maybe
import Data.Monoid
import Data.Word
import Foreign
import System.IO.Unsafe (unsafePerformIO)

import qualified Codec.Picture.Metadata as CM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Foldable as F
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

import Codec.Picture.Blp.Internal.Convert
import Codec.Picture.Blp.Internal.Data

-- | Convert spare BLP structure into compact stream of bytes
encodeBlp :: Int -> BlpStruct -> ByteString
encodeBlp numMips = runPut . blpEncoder numMips

-- | Raw encoder for BLP
blpEncoder :: Int -> BlpStruct -> Put
blpEncoder numMips BlpStruct{..} = do
  putBlpVersion
  putCompression
  putFlags
  putWidth
  putHeight
  putPictureType
  putPictureSubType
  putMipMapOffsets
  putMipMapSizes
  putBlpExt
  where
    putBlpVersion = putByteString "BLP1"
    putCompression = putWord32le $ case blpCompression of
      BlpCompressionJPEG -> 0
      BlpCompressionUncompressed -> 1
    putFlags = let
      addFlag acc flag = (acc +) $ case flag of
         BlpFlagAlphaChannel -> 1 `shiftL` 3
      in putWord32le $ F.foldl' addFlag 0 blpFlags
    putWidth = putWord32le blpWidth
    putHeight = putWord32le blpHeight
    putPictureType = putWord32le $ case blpPictureType of
      JPEGType -> 2
      UncompressedWithAlpha -> 3
      -- UncompressedWithAlpha -> 4
      UncompressedWithoutAlpha -> 5
    putPictureSubType = putWord32le blpPictureSubType
    headerSize = 4 -- BLP1
      + 4 -- Compression
      + 4 -- Flags
      + 4 -- Width
      + 4 -- Height
      + 4 -- Picture type
      + 4 -- Picture subtype
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
        BlpUncompressed1{..} -> let
          mkOffsetSizeU (indbs, alpbs) (!offset, !acc) = (offset + BS.length indbs + BS.length alpbs, (fromIntegral offset, fromIntegral $ BS.length indbs + BS.length alpbs) : acc)
          in F.foldr' mkOffsetSizeU (uncomprStartOffset, []) $ reverse blpU1MipMaps
        BlpUncompressed2{..} -> F.foldr' mkOffsetSize (uncomprStartOffset, []) $ reverse blpU2MipMaps
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
       BlpUncompressed1{..} -> do
         traverse_ putRgba8 . ensureLengthV 256 (PixelRGBA8 0 0 0 0) $ blpU1Palette
         traverse_ (\(indbs, alpbs) -> putByteString indbs >> putByteString alpbs) $ take numMips blpU1MipMaps
       BlpUncompressed2{..} -> do
         traverse_ putRgba8 . ensureLengthV 256 (PixelRGBA8 0 0 0 0) $ blpU2Palette
         traverse_ putByteString $ take numMips blpU2MipMaps

-- | Return 'True' if given picture format has alpha channel
hasAlpha :: DynamicImage -> Bool
hasAlpha img = case img of
  ImageYA8 _ -> True
  ImageYA16 _ -> True
  ImageRGBA8 _ -> True
  ImageRGBA16 _ -> True
  _ -> False

-- | Convert to BLP structure some image with given BLP options and quality (for JPEG compression)
toBlpStruct :: BlpCompression -> Int -> Int -> DynamicImage -> BlpStruct
toBlpStruct compression quality numMips img = BlpStruct {
    blpCompression = compression
  , blpFlags = if hasAlpha img
      then [BlpFlagAlphaChannel]
      else []
  , blpWidth = fromIntegral $ dynamicMap imageWidth img
  , blpHeight = fromIntegral $ dynamicMap imageHeight img
  , blpPictureType = pictype
  , blpPictureSubType = 5 -- world edit use this for war3mapMap.blp
  , blpExt = toBlpExt compression pictype quality numMips img'
  }
  where
    pictype = if hasAlpha img
      then UncompressedWithAlpha
      else UncompressedWithoutAlpha
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
-- The scale procedure assumes that original image has power of 2 sides, that allows to simply pick
-- average of 4 pixels.
createMipMaps :: Image PixelRGBA8 -> [Image PixelRGBA8]
createMipMaps img = img : go img
  where
    avg' v1 v2 v3 v4 = let
      v = fromIntegral v1 + fromIntegral v2 + fromIntegral v3 + fromIntegral v4 :: Double
      in round $ v / 4
    avg i x y = let
      PixelRGBA8 p00r p00g p00b p00a = pixelAt i (x*2) (y*2)
      PixelRGBA8 p10r p10g p10b p10a = pixelAt i (x*2 + 1) (y*2)
      PixelRGBA8 p01r p01g p01b p01a = pixelAt i (x*2) (y*2 + 1)
      PixelRGBA8 p11r p11g p11b p11a = pixelAt i (x*2 + 1) (y*2 + 1)
      in PixelRGBA8 (avg' p00r p10r p01r p11r) (avg' p00g p10g p01g p11g) (avg' p00b p10b p01b p11b) (avg' p00a p10a p01a p11a)
    power2Scale i = generateImage (avg i) (downgrade $ imageWidth i) (downgrade $ imageHeight i)
    downgrade v = max 1 $ v `div` 2
    go i | imageWidth i <= 1 || imageHeight i <= 1 = []
         | otherwise = let i' = power2Scale i in i' : go i'

-- | Scale image to form the sequence of mipmaps. The first element is always the original picture.
--
-- The scale procedure assumes that original image has power of 2 sides, that allows to simply pick
-- 1 of 4 pixels.
createMipMapsIndexed :: Pixel a => Image a -> [Image a]
createMipMapsIndexed img = img : go img
  where
    power2Scale i = generateImage (\x y -> pixelAt i (x*2) (y*2)) (downgrade $ imageWidth i) (downgrade $ imageHeight i)
    downgrade v = max 1 $ v `div` 2
    go i | imageWidth i <= 1 || imageHeight i <= 1 = []
         | otherwise = let i' = power2Scale i in i' : go i'

-- | Convert picture to BLP payload
toBlpExt :: BlpCompression -> BlpPictureType -> Int -> Int -> Image PixelRGBA8 -> BlpExt
toBlpExt compr pictype quality numMips img = case compr of
  BlpCompressionJPEG -> toBlpJpg (fromIntegral quality) numMips hasAlpha img
  BlpCompressionUncompressed -> case pictype of
    UncompressedWithAlpha -> toBlpUncompressed1 numMips img
    UncompressedWithoutAlpha -> toBlpUncompressed2 numMips img
    JPEGType -> toBlpUncompressed2 numMips img -- Consider this as without alpha
  where
    hasAlpha = case pictype of
      UncompressedWithAlpha -> True
      _ -> False

-- | Convert picture to BLP JPEG and create mipmaps
toBlpJpg :: Word8 -> Int -> Bool -> Image PixelRGBA8 -> BlpExt
toBlpJpg quality numMips hasAlpha img = BlpJpeg {
    blpJpegHeader = header
  , blpJpegData = mipmapsRawWithoutHeader
  }
  where
    processAlpha :: Image PixelRGBA8 -> Image PixelRGBA8
    processAlpha = pixelMap $ \p@(PixelRGBA8 r g b a) -> if hasAlpha then p else PixelRGBA8 r g b 255

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
scanHeader maxheader [x] = BS.take maxheader x
scanHeader maxheader mipmaps = go mipmaps mempty
  where
    go !mps !acc = let
      unconses = BS.uncons <$> mps :: [Maybe (Word8, BS.ByteString)]
      heads = fmap fst <$> unconses :: [Maybe Word8]
      tails = catMaybes $ fmap snd <$> unconses :: [BS.ByteString]
      hitEmpty = any isNothing heads
      firstByte = case heads of
        (Just v : _) -> v
        _ -> 0
      allEqual = all (Just firstByte ==) heads
      in if | hitEmpty -> acc
            | maxheader >= BS.length acc -> acc
            | allEqual -> go tails (acc `BS.snoc` firstByte)
            | otherwise -> acc

-- | Helper to quantise colors to 256 colour pallete
makePallette :: Image PixelRGBA8 -> (Image Pixel8, Palette)
makePallette = palettize defaultPaletteOptions . pixelMap dropTransparency

instance Storable PixelRGBA8 where
  sizeOf _ = sizeOf (undefined :: Word32)
  alignment _ = alignment (undefined :: Word32)
  peek ptr = unpackPixel <$> peek (castPtr ptr :: Ptr Word32)
  poke ptr px = poke (castPtr ptr :: Ptr Word32) . packPixel $ px

-- | Convert palette to format that we need for BLP
convertPalette :: Palette -> V.Vector PixelRGBA8
convertPalette = V.convert . VS.unsafeCast . imageData . pixelMap ((\(PixelRGBA8 r g b a) -> PixelRGBA8 b g r a) . promotePixel :: PixelRGB8 -> PixelRGBA8)

-- | Convert indexed image to raw bytestring
convertIndexed :: Image Pixel8 -> BS.ByteString
convertIndexed img = let
  (fptr, l) = VS.unsafeToForeignPtr0 . imageData $ img
  in unsafePerformIO $ withForeignPtr fptr $ \ptr -> BS.unsafePackCStringLen (castPtr ptr, l)

-- | Convert picture to BLP uncompressed with alpha and create mipmaps
toBlpUncompressed1 :: Int -> Image PixelRGBA8 -> BlpExt
toBlpUncompressed1 numMips img =  BlpUncompressed1 {
    blpU1Palette = convertPalette palette
  , blpU1MipMaps = fmap convertIndexed mipmaps `zip` fmap convertIndexed alphaMaps
  }
  where
    img'    :: Image Pixel8
    palette :: Palette
    (img', palette) = makePallette img

    alphaImg :: Image Pixel8
    alphaImg = extractComponent PlaneAlpha img

    mipmaps :: [Image Pixel8]
    mipmaps = fakeMipMaps numMips $ createMipMapsIndexed img'

    alphaMaps :: [Image Pixel8]
    alphaMaps = fakeMipMaps numMips $ createMipMapsIndexed alphaImg

-- | Convert picture to BLP uncompressed without alpha and create mipmaps
toBlpUncompressed2 :: Int -> Image PixelRGBA8 -> BlpExt
toBlpUncompressed2 numMips img = BlpUncompressed2 {
    blpU2Palette = convertPalette palette
  , blpU2MipMaps = convertIndexed <$> mipmaps
  }
  where
    img'    :: Image Pixel8
    palette :: Palette
    (img', palette) = makePallette img

    mipmaps :: [Image Pixel8]
    mipmaps = fakeMipMaps numMips $ createMipMapsIndexed img'
