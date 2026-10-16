{-|
Building and serialising BLP1 files: palette quantisation, the mip chain, the
shared JPEG header and the binary layout.

This is an internal module: its API may change between minor versions.
-}
module Codec.Picture.Blp.Internal.Encoder(
    encodeBlp
  , blpEncoder
  , toBlpStruct
  , toBlp2Struct
  , encodeBlp2
  , blp2Encoder
  , shrinkTo
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
import Data.List (sortOn)
import Data.Word

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable.Mutable as MVS

import Codec.Picture.Blp.Internal.Convert
import Codec.Picture.Blp.Internal.Data
import Codec.Picture.Blp.Internal.Dxt (DxtFormat(..), encodeDxt)
import Codec.Picture.Blp.Types

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

-- | Build the BLP structure for an image. The number of mip levels is clamped
-- to between 1 and the image's full chain, at most 16. The image's size is
-- not checked, and 'blpMaxSize' is applied by the caller (see 'shrinkTo').
toBlpStruct :: BlpEncodeOptions -> DynamicImage -> Either BlpEncodeError BlpStruct
toBlpStruct BlpEncodeOptions{..} img = do
  ext <- case blpFormat of
    BlpFormatJpeg quality -> Right $ toBlpJpg (fromIntegral quality) numLevels alpha rgba
    BlpFormatPaletted _ -> Right $ toBlpPaletted blpDither alpha numLevels (convertRGB8 img) rgba
    other -> Left $ BlpUnsupportedFormat Blp1 other
  return BlpStruct {
      blpAlphaBits = if alpha then 8 else 0
    , blpWidth = fromIntegral width
    , blpHeight = fromIntegral height
    -- Readers that follow the old description of the format pick alpha by
    -- this field (3 or 4 with alpha, 5 without), so keep it consistent.
    , blpExtra = if alpha then 3 else 5
    -- WorldEdit writes 5; a single level says it has no mipmaps.
    , blpHasMipmaps = if numLevels > 1 then 5 else 0
    , blpExt = ext
    }
  where
    -- JPEG keeps alpha only if some pixel is not fully opaque.
    alpha = case blpFormat of
      BlpFormatJpeg _ -> hasAlpha img && anyTransparent rgba
      BlpFormatPaletted withAlpha -> withAlpha
      _ -> False
    -- Only built when needed: paletted textures without alpha go from RGB.
    rgba = convertRGBA8 img
    width = dynamicMap imageWidth img
    height = dynamicMap imageHeight img
    numLevels = max 1 $ min (mipChainLength width height) blpMipLevels

-- | Build the BLP2 structure for an image, as 'toBlpStruct' does for BLP1.
-- The header values follow files from World of Warcraft: alpha depth 0 or
-- 1 for DXT1, 8 for DXT3, DXT5 and BGRA, 8 or 0 for a palette; alpha type
-- 0, 1 and 7 for the DXT formats and 8 otherwise.
toBlp2Struct :: BlpEncodeOptions -> DynamicImage -> Either BlpEncodeError Blp2Struct
toBlp2Struct BlpEncodeOptions{..} img = case blpFormat of
  BlpFormatPaletted withAlpha ->
    let (palette, mips) = quantiseLevels blpDither withAlpha numLevels (convertRGB8 img) rgba
    in Right $ struct (if withAlpha then 8 else 0) 8 (Blp2Paletted palette) mips
  BlpFormatDxt1 ->
    let punch = hasAlpha img && anyBelowHalf rgba
    in Right $ struct (if punch then 1 else 0) 0 (Blp2Dxt (Dxt1 punch)) (encodeDxt (Dxt1 punch) <$> levels)
  BlpFormatDxt3 -> Right $ struct 8 1 (Blp2Dxt Dxt3) (encodeDxt Dxt3 <$> levels)
  BlpFormatDxt5 -> Right $ struct 8 7 (Blp2Dxt Dxt5) (encodeDxt Dxt5 <$> levels)
  BlpFormatBgra -> Right $ struct 8 8 Blp2Bgra (bgraBytes <$> levels)
  other -> Left $ BlpUnsupportedFormat Blp2 other
  where
    rgba = convertRGBA8 img
    width = dynamicMap imageWidth img
    height = dynamicMap imageHeight img
    numLevels = max 1 $ min (mipChainLength width height) blpMipLevels
    levels = take numLevels (createMipMaps rgba)
    struct depth alphaType content mips = Blp2Struct {
        blp2AlphaDepth = depth
      , blp2AlphaType = alphaType
      , blp2HasMips = if numLevels > 1 then 1 else 0
      , blp2Width = fromIntegral width
      , blp2Height = fromIntegral height
      , blp2Content = content
      , blp2MipMaps = mips
      }

-- | Whether any pixel's alpha is below 128, so DXT1 needs 1-bit alpha.
anyBelowHalf :: Image PixelRGBA8 -> Bool
anyBelowHalf img = VS.any id $ VS.generate (imageWidth img * imageHeight img) (\i -> VS.unsafeIndex (imageData img) (4 * i + 3) < 128)

-- | Pixels as B, G, R and A.
bgraBytes :: Image PixelRGBA8 -> BS.ByteString
bgraBytes img = BS.pack . concat $ [[b, g, r, a] | PixelRGBA8 r g b a <- pixels]
  where pixels = [pixelAt img x y | y <- [0 .. imageHeight img - 1], x <- [0 .. imageWidth img - 1]]

-- | Serialise a BLP2 structure. At most 16 mip levels are written, unused
-- slots are zero, and the palette is zeros unless the content is paletted.
encodeBlp2 :: Blp2Struct -> ByteString
encodeBlp2 = runPut . blp2Encoder

-- | Raw encoder for BLP2.
blp2Encoder :: Blp2Struct -> Put
blp2Encoder Blp2Struct{..} = do
  putByteString "BLP2"
  putWord32le 1
  putWord8 $ case blp2Content of
    Blp2Paletted _ -> 1
    Blp2Dxt _ -> 2
    Blp2Bgra -> 3
  putWord8 blp2AlphaDepth
  putWord8 blp2AlphaType
  putWord8 blp2HasMips
  putWord32le blp2Width
  putWord32le blp2Height
  traverse_ putWord32le . slots $ take (length levels) (scanl (+) contentStart levelSizes)
  traverse_ putWord32le $ slots levelSizes
  traverse_ putRgba8 . V.take 256 $ palette <> V.replicate 256 (PixelRGBA8 0 0 0 0)
  traverse_ putByteString levels
  where
    contentStart = 148 + 4 * 256
    levels = take 16 blp2MipMaps
    levelSizes = BS.length <$> levels
    palette = case blp2Content of
      Blp2Paletted p -> p
      _ -> V.empty
    slots xs = take 16 $ (fromIntegral <$> xs) ++ repeat 0
    putRgba8 (PixelRGBA8 r g b a) = putWord8 r >> putWord8 g >> putWord8 b >> putWord8 a

-- | Whether any pixel is less than fully opaque.
anyTransparent :: Image PixelRGBA8 -> Bool
anyTransparent img = go 3
  where
    src = imageData img
    go i
      | i >= VS.length src = False
      | VS.unsafeIndex src i /= 255 = True
      | otherwise = go (i + 4)

-- | Halve an image, as its mipmaps are made, until its larger side is at most
-- the given number of pixels. An image without alpha stays without alpha.
shrinkTo :: Int -> DynamicImage -> DynamicImage
shrinkTo limit img
  | max width height <= limit = img
  | otherwise = case dropWhile tooLarge (createMipMaps (convertRGBA8 img)) of
      level : _ -> if hasAlpha img then ImageRGBA8 level else ImageRGB8 (convertRGB8 (ImageRGBA8 level))
      [] -> img
  where
    width = dynamicMap imageWidth img
    height = dynamicMap imageHeight img
    tooLarge level = max (imageWidth level) (imageHeight level) > limit

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

-- | Quantise to a 256 colour palette, with or without dithering.
makePallette :: Bool -> Image PixelRGB8 -> (Image Pixel8, Palette)
makePallette dither = palettize defaultPaletteOptions { enableImageDithering = dither }

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
toBlpPaletted :: Bool -> Bool -> Int -> Image PixelRGB8 -> Image PixelRGBA8 -> BlpExt
toBlpPaletted dither keepAlpha numLevels rgb rgba =
  uncurry BlpPaletted (quantiseLevels dither keepAlpha numLevels rgb rgba)

-- | The palette and the raw mip levels of 'toBlpPaletted', shared with BLP2.
quantiseLevels :: Bool -> Bool -> Int -> Image PixelRGB8 -> Image PixelRGBA8 -> (V.Vector PixelRGBA8, [BS.ByteString])
quantiseLevels dither keepAlpha numLevels rgb rgba =
  ( convertPalette palette
  , if keepAlpha
      then zipWith (<>) (convertIndexed <$> mipmaps) (convertIndexed <$> alphaMaps)
      else convertIndexed <$> mipmaps
  )
  where
    img'    :: Image Pixel8
    palette :: Palette
    -- Fully transparent pixels are made black, so the colours hidden under
    -- them take no palette entries.
    (img', palette) = makePallette dither $ if keepAlpha then fillTransparent rgba else rgb

    -- Smaller levels are averaged in RGBA and mapped to the nearest palette
    -- entry; picking one index per block would sample the dithering noise.
    mipmaps :: [Image Pixel8]
    mipmaps = take numLevels $ img' : (nearestIndices palette <$> drop 1 (createMipMaps averaged))
    averaged = if keepAlpha then rgba else opaqueRGBA8 rgba

    -- Alpha is coverage, not an index, so its mipmaps are averaged.
    alphaMaps :: [Image Pixel8]
    alphaMaps = take numLevels $ extractComponent PlaneAlpha <$> createMipMaps rgba

-- | The RGB of an image, with every fully transparent pixel made black.
-- Whatever colour those pixels had is invisible, and leaving it in would
-- spend palette entries on it; a single colour costs at most one entry.
fillTransparent :: Image PixelRGBA8 -> Image PixelRGB8
fillTransparent = pixelMap $ \(PixelRGBA8 r g b a) -> if a == 0 then PixelRGB8 0 0 0 else PixelRGB8 r g b

-- | Index of the palette entry nearest to each pixel's colour (squared RGB
-- distance; alpha is stored separately; ties go to the lower index).
--
-- The entries are searched in order of red, outwards from the pixel's red,
-- and a direction stops once the red difference alone is no better than the
-- best match. That gives the same answer as trying every entry, with far
-- fewer tries.
nearestIndices :: Palette -> Image PixelRGBA8 -> Image Pixel8
nearestIndices palette img = pixelMap nearest img
  where
    entries = imageData palette
    count = VS.length entries `div` 3
    channel i k = fromIntegral (VS.unsafeIndex entries (i * 3 + k)) :: Int
    -- Entry indices sorted by red, and their reds.
    byRed = VU.fromList . map snd . sortOn fst $ [((channel i 0, i), i) | i <- [0 .. count - 1]]
    reds = VU.map (`channel` 0) byRed
    nearest (PixelRGBA8 r8 g8 b8 _) = fromIntegral . snd $ down (start - 1) (up start (maxBound, 0))
      where
        r = fromIntegral r8 :: Int
        g = fromIntegral g8 :: Int
        b = fromIntegral b8 :: Int
        -- First position whose red is at least the pixel's.
        start = search 0 count
        search lo hi
          | lo >= hi = lo
          | VU.unsafeIndex reds mid < r = search (mid + 1) hi
          | otherwise = search lo mid
          where mid = (lo + hi) `div` 2
        consider j best@(bestDist, bestIndex) = let
          i = VU.unsafeIndex byRed j
          dr = channel i 0 - r
          dg = channel i 1 - g
          db = channel i 2 - b
          dist = dr * dr + dg * dg + db * db
          in if dist < bestDist || (dist == bestDist && i < bestIndex) then (dist, i) else best
        up j best@(bestDist, _)
          | j >= count = best
          | let dr = VU.unsafeIndex reds j - r, dr * dr > bestDist = best
          | otherwise = up (j + 1) (consider j best)
        down j best@(bestDist, _)
          | j < 0 = best
          | let dr = r - VU.unsafeIndex reds j, dr * dr > bestDist = best
          | otherwise = down (j - 1) (consider j best)
