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
  ) where

import Codec.Picture
import Data.ByteString (ByteString)
import Data.Monoid
import Data.Word

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
  if null is then Left "No data in BLP"
    else return $ head is

-- | Decodes BLP and returns original image plus all mipmaps
decodeBlpMipmaps :: ByteString -> Either String [DynamicImage]
decodeBlpMipmaps bs = do
  blp <- parseBlp bs
  case blpExt blp of
    BlpJpeg {..} -> do
      let jpegs = (blpJpegHeader <>) `fmap` blpJpegData
      mips <- mapM decodeJpeg jpegs
      return $ toPngRepresentable <$> mips

    BlpUncompressed1 {..} -> do
      let mkImage mip = ImageRGBA8 $ generateImage (gen mip) (fromIntegral $ blpWidth blp) (fromIntegral $ blpHeight blp)
      return $ mkImage `fmap` blpU1MipMaps
      where
      palette :: Word8 -> PixelRGBA8
      palette i = blpU1Palette V.! fromIntegral i

      makeIndex x y = y*(fromIntegral $ blpWidth blp)+x
      takeColor mip x y = palette $ fst mip `BS.index` makeIndex x y
      takeAlpha mip x y = snd mip `BS.index` makeIndex x y
      gen mip x y = let
        PixelRGBA8 r g b _ = takeColor mip x y
        a = takeAlpha mip x y
        in PixelRGBA8 b g r a

    BlpUncompressed2 {..} -> do
      let mkImage mip = ImageRGBA8 $ generateImage (gen mip) (fromIntegral $ blpWidth blp) (fromIntegral $ blpHeight blp)
      return $ mkImage `fmap` blpU2MipMaps
      where
      palette :: Word8 -> PixelRGBA8
      palette i = blpU2Palette V.! fromIntegral i

      makeIndex x y = y*(fromIntegral $ blpWidth blp)+x
      takeColor mip x y = palette $ mip `BS.index` makeIndex x y
      gen mip x y = let
        PixelRGBA8 r g b a = takeColor mip x y
        in PixelRGBA8 b g r (255 - a)

writeBlpJpeg :: FilePath -> Int -> DynamicImage -> IO ()
writeBlpJpeg fp quality img = do
  let bs = encodeBlpJpeg quality img
  BSL.writeFile fp bs

writeBlpUncompressedWithAlpha :: FilePath -> DynamicImage -> IO ()
writeBlpUncompressedWithAlpha fp img = do
  let bs = encodeBlpUncompressedWithAlpha img
  BSL.writeFile fp bs

writeBlpUncompressedWithoutAlpha :: FilePath -> DynamicImage -> IO ()
writeBlpUncompressedWithoutAlpha fp img = do
  let bs = encodeBlpUncompressedWithoutAlpha img
  BSL.writeFile fp bs

encodeBlpJpeg :: Int -> DynamicImage -> BSL.ByteString
encodeBlpJpeg quality = encodeBlp . toBlpStruct BlpCompressionJPEG JPEGType quality

encodeBlpUncompressedWithAlpha :: DynamicImage -> BSL.ByteString
encodeBlpUncompressedWithAlpha = encodeBlp . toBlpStruct BlpCompressionUncompressed UncompressedWithAlpha 100

encodeBlpUncompressedWithoutAlpha :: DynamicImage -> BSL.ByteString
encodeBlpUncompressedWithoutAlpha = encodeBlp . toBlpStruct BlpCompressionUncompressed UncompressedWithoutAlpha 100
