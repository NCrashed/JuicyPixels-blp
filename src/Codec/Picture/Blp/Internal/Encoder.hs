module Codec.Picture.Blp.Internal.Encoder(
    encodeBlp
  , blpEncoder
  , createMipMaps
  , scanHeader
  , toBlpStruct
  , toBlpExt
  ) where

import Codec.Picture
import Codec.Picture.ColorQuant
import Codec.Picture.Jpg
import Codec.Picture.ScaleDCT
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

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Foldable as F
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

import Codec.Picture.Blp.Internal.Convert
import Codec.Picture.Blp.Internal.Data

-- | Convert spare BLP structure into compact stream of bytes
encodeBlp :: BlpStruct -> ByteString
encodeBlp = runPut . blpEncoder

-- | Raw encoder for BLP
blpEncoder :: BlpStruct -> Put
blpEncoder BlpStruct{..} = do
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
    ensureLength  n a xs = if   length xs < n then xs ++   replicate (n -   length xs) a else   take n xs
    ensureLengthV n a xs = if V.length xs < n then xs <> V.replicate (n - V.length xs) a else V.take n xs
    putMipMapOffsets = traverse_ putWord32le . ensureLength 16 0 . fmap fst $ mipmaps
    putMipMapSizes = traverse_ putWord32le . ensureLength 16 0 . fmap snd $ mipmaps
    putRgba8 (PixelRGBA8 r g b a) = putWord8 r >> putWord8 g >> putWord8 b >> putWord8 a
    putBlpExt = case blpExt of -- TODO: check sync of compression flag and BlpExt
       BlpJpeg{..} -> do
         putWord32le $ fromIntegral $ BS.length blpJpegHeader
         putByteString blpJpegHeader
         traverse_ putByteString blpJpegData
       BlpUncompressed1{..} -> do
         traverse_ putRgba8 . ensureLengthV 256 (PixelRGBA8 0 0 0 0) $ blpU1Palette
         traverse_ (\(indbs, alpbs) -> putByteString indbs >> putByteString alpbs) blpU1MipMaps
       BlpUncompressed2{..} -> do
         traverse_ putRgba8 . ensureLengthV 256 (PixelRGBA8 0 0 0 0) $ blpU2Palette
         traverse_ putByteString blpU2MipMaps

-- | Return 'True' if given picture format has alpha channel
hasAlpha :: DynamicImage -> Bool
hasAlpha img = case img of
  ImageYA8 _ -> True
  ImageYA16 _ -> True
  ImageRGBA8 _ -> True
  ImageRGBA16 _ -> True
  _ -> False

-- | Convert to BLP structure some image with given BLP options and quality (for JPEG compression)
toBlpStruct :: BlpCompression -> BlpPictureType -> Int -> DynamicImage -> BlpStruct
toBlpStruct compression pictype quality img = BlpStruct {
    blpCompression = compression
  , blpFlags = case pictype of
     JPEGType -> if hasAlpha img then [BlpFlagAlphaChannel] else []
     UncompressedWithAlpha -> [BlpFlagAlphaChannel]
     UncompressedWithoutAlpha -> []
  , blpWidth = fromIntegral $ dynamicMap imageWidth img
  , blpHeight = fromIntegral $ dynamicMap imageHeight img
  , blpPictureType = pictype
  , blpPictureSubType = 0
  , blpExt = toBlpExt compression pictype quality img'
  }
  where
    img' = convertRGBA8 img

-- | Scale image to form the sequence of mipmaps. The first element is always the original picture.
createMipMaps :: Image PixelRGBA8 -> [Image PixelRGBA8]
createMipMaps img = img : go img
  where
    downgrade v = max 1 $ v `div` 2
    go i | imageWidth i <= 1 && imageHeight i <= 1 = []
         | otherwise = let i' = scale (downgrade $ imageWidth i, downgrade $ imageHeight i) i in i' : go i'

-- | Scale image to form the sequence of mipmaps. The first element is always the original picture.
--
-- The scale procedure assumes that original image has power of 2 sides, that allows to simply pick
-- 1 of 4 pixels for indexed images.
createMipMapsPowerOf2 :: Pixel a => Image a -> [Image a]
createMipMapsPowerOf2 img = img : go img
  where
    power2Scale i = generateImage (\x y -> pixelAt i (x*2) (y*2)) (downgrade $ imageWidth i) (downgrade $ imageHeight i)
    downgrade v = max 1 $ v `div` 2
    go i | imageWidth i <= 1 && imageHeight i <= 1 = []
         | otherwise = let i' = power2Scale i in i' : go i'

-- | Convert picture to BLP payload
toBlpExt :: BlpCompression -> BlpPictureType -> Int -> Image PixelRGBA8 -> BlpExt
toBlpExt compr pictype quality img = case compr of
  BlpCompressionJPEG -> toBlpJpg (fromIntegral quality) img
  BlpCompressionUncompressed -> case pictype of
    UncompressedWithAlpha -> toBlpUncompressed1 img
    UncompressedWithoutAlpha -> toBlpUncompressed2 img
    JPEGType -> toBlpUncompressed2 img -- Consider this as without alpha

-- | Convert picture to BLP JPEG and create mipmaps
toBlpJpg :: Word8 -> Image PixelRGBA8 -> BlpExt
toBlpJpg quality img = BlpJpeg {
    blpJpegHeader = header
  , blpJpegData = mipmapsRawWithoutHeader
  }
  where
    mipmaps :: [Image PixelCMYK8]
    mipmaps = toBlpCMYK8 <$> createMipMaps img

    mipmapsRaw :: [BS.ByteString]
    mipmapsRaw = toStrict . encodeDirectJpegAtQualityWithMetadata quality mempty <$> mipmaps

    header :: BS.ByteString
    header = scanHeader mipmapsRaw

    mipmapsRawWithoutHeader :: [BS.ByteString]
    mipmapsRawWithoutHeader = BS.drop (BS.length header) <$> mipmapsRaw

-- | Manually scan shared prefix of each mipmap
scanHeader :: [BS.ByteString] -> BS.ByteString
scanHeader [] = mempty
scanHeader [x] = mempty
scanHeader mipmaps = go mipmaps mempty
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
toBlpUncompressed1 :: Image PixelRGBA8 -> BlpExt
toBlpUncompressed1 img =  BlpUncompressed1 {
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
    mipmaps = createMipMapsPowerOf2 img'

    alphaMaps :: [Image Pixel8]
    alphaMaps = createMipMapsPowerOf2 alphaImg

-- | Convert picture to BLP uncompressed without alpha and create mipmaps
toBlpUncompressed2 :: Image PixelRGBA8 -> BlpExt
toBlpUncompressed2 img = BlpUncompressed2 {
    blpU2Palette = convertPalette palette
  , blpU2MipMaps = convertIndexed <$> mipmaps
  }
  where
    img'    :: Image Pixel8
    palette :: Palette
    (img', palette) = makePallette img

    mipmaps :: [Image Pixel8]
    mipmaps = createMipMapsPowerOf2 img'
