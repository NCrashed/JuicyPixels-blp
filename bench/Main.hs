-- | Benchmarks for decoding and encoding BLP textures.
--
-- Inputs are generated in code: a gradient with deterministic noise, so that
-- JPEG and palette quantisation have real work to do. Run with
--
-- > cabal bench --benchmark-options '-p decode'
--
-- and compare against an earlier run with @--csv old.csv@ / @--baseline old.csv@.
module Main(main) where

import Codec.Picture
import Codec.Picture.Blp
import Codec.Picture.Blp.Internal.Convert (jpegAlphaIsUsable)
import Codec.Picture.Blp.Internal.Dxt (DxtFormat(..), decodeDxt, dxtLevelBytes)
import Codec.Picture.Blp.Internal.Encoder (createMipMaps)
import Codec.Picture.Jpg (encodeDirectJpegAtQualityWithMetadata)
import Control.DeepSeq
import Data.Bits
import Data.ByteString (ByteString)
import Data.Word
import Test.Tasty.Bench

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

main :: IO ()
main = defaultMain
  [ bgroup "decode"
    [ decodeGroup "jpeg alpha" (jpeg True) [64, 512, 2048]
    , decodeGroup "jpeg opaque" (jpeg False) [512, 2048]
    , decodeGroup "paletted alpha8" (paletted True) [64, 512, 2048]
    , decodeGroup "paletted alpha4" (setAlphaBits 4 . paletted True) [512]
    , decodeGroup "paletted alpha1" (setAlphaBits 1 . paletted True) [512]
    , decodeGroup "paletted opaque" (paletted False) [512, 2048]
    ]
  , bgroup "encode"
    [ encodeGroup "jpeg full chain" (BlpFormatJpeg 90) 16 texture [64, 512, 1024]
    , encodeGroup "jpeg one level" (BlpFormatJpeg 90) 1 texture [512, 2048]
    , encodeGroup "paletted alpha" (BlpFormatPaletted True) 16 texture [64, 512]
    , encodeGroup "paletted opaque from RGB8" (BlpFormatPaletted False) 16 (\n -> ImageRGB8 . convertRGB8 $ texture n) [512]
    , env (return . force $ texture2 1024 128) $ \img ->
        bench "jpeg full chain/1024x128" $ nf (encodeWith (BlpFormatJpeg 90) 16) img
    ]
  , bgroup "internals"
    [ env (return . force $ rgba 2048) $ \img ->
        bench "createMipMaps/2048" $ nf (map imageData . createMipMaps) img
    , env (return . force $ cmykLevel (rgba 2048)) $ \img ->
        bench "jpegAlphaIsUsable/2048 varying" $ nf jpegAlphaIsUsable img
    , env (return . force $ cmykLevel (convertRGBA8 (opaque 2048))) $ \img ->
        bench "jpegAlphaIsUsable/2048 opaque" $ nf jpegAlphaIsUsable img
    , env (return . force $ dxtBlocks (Dxt1 True) 2048) $ \blocks ->
        bench "decodeDxt/2048 dxt1" $ nf (fmap imageData . decodeDxt (Dxt1 True) 2048 2048) blocks
    , env (return . force $ dxtBlocks Dxt5 2048) $ \blocks ->
        bench "decodeDxt/2048 dxt5" $ nf (fmap imageData . decodeDxt Dxt5 2048 2048) blocks
    ]
  ]

-- | Decode mip 0 only, and the whole chain, of a file made for each size.
decodeGroup :: String -> (Int -> ByteString) -> [Int] -> Benchmark
decodeGroup name mkFile sizes = bgroup name
  [ env (return . force $ mkFile n) $ \file -> bgroup (show n)
      [ bench "mip 0" $ nf (fmap pixels . decodeBlp) file
      , bench "all levels" $ nf (fmap (map pixels) . decodeBlpMipmaps) file
      ]
  | n <- sizes ]
  where
    pixels = imageData

-- | Encode an image of each size with the given format and level count.
encodeGroup :: String -> BlpFormat -> Int -> (Int -> DynamicImage) -> [Int] -> Benchmark
encodeGroup name format levels mkImage sizes = bgroup name
  [ env (return . force $ mkImage n) $ \img ->
      bench (show n) $ nf (encodeWith format levels) img
  | n <- sizes ]

encodeWith :: BlpFormat -> Int -> DynamicImage -> BSL.ByteString
encodeWith format levels = either (error . show) id . encodeBlpWith defaultBlpEncodeOptions { blpFormat = format, blpMipLevels = levels }

-- | Square test texture with varying alpha.
texture :: Int -> DynamicImage
texture n = texture2 n n

texture2 :: Int -> Int -> DynamicImage
texture2 w h = ImageRGBA8 $ generateImage px w h
  where
    px x y = let
      noise = fromIntegral (hash (x * 7919 + y * 104729)) :: Word8
      in PixelRGBA8 (fromIntegral (x * 255 `div` w) + noise `shiftR` 3)
                    (fromIntegral (y * 255 `div` h) + noise `shiftR` 4)
                    noise
                    (fromIntegral ((x + y) * 255 `div` (w + h)))
    hash v = (v * 2654435761) `shiftR` 13 .&. 0xFF

rgba :: Int -> Image PixelRGBA8
rgba = convertRGBA8 . texture

opaque :: Int -> DynamicImage
opaque = ImageRGBA8 . pixelMap (\(PixelRGBA8 r g b _) -> PixelRGBA8 r g b 255) . rgba

jpeg :: Bool -> Int -> ByteString
jpeg withAlpha n = BSL.toStrict . encodeWith (BlpFormatJpeg 90) 16 $
  if withAlpha then texture n else opaque n

paletted :: Bool -> Int -> ByteString
paletted withAlpha n = BSL.toStrict $ encodeWith (BlpFormatPaletted withAlpha) 16 (texture n)

-- | Rewrite the alphaBits header field; an 8-bit alpha list is long enough
-- for 1 and 4 bits.
setAlphaBits :: Word32 -> ByteString -> ByteString
setAlphaBits bits file = BS.take 8 file <> BS.pack [fromIntegral (bits `shiftR` (8 * i)) | i <- [0 .. 3]] <> BS.drop 12 file

-- | A texture as JuicyPixels decodes a BLP JPEG level, before conversion.
-- An opaque one is the worst case for the alpha check: it scans every pixel.
cmykLevel :: Image PixelRGBA8 -> DynamicImage
cmykLevel img = either error id . decodeJpeg . BSL.toStrict $
  encodeDirectJpegAtQualityWithMetadata 90 mempty (pixelMap (\(PixelRGBA8 r g b a) -> PixelCMYK8 b g r a) img)

-- | Pseudo-random blocks for an n x n DXT image: every colour and alpha
-- mode occurs.
dxtBlocks :: DxtFormat -> Int -> ByteString
dxtBlocks fmt n = BS.pack [fromIntegral ((i * 2654435761) `shiftR` 13) | i <- [0 .. dxtLevelBytes fmt n n - 1]]
