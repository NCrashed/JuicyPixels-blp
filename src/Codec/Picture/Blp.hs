{-|
Decoding and encoding of BLP, the texture format of Warcraft III (BLP1) and
World of Warcraft (BLP2), on top of JuicyPixels. BLP1 is read and written,
BLP2 is read.

BLP1 comes in two flavours: JPEG compressed and uncompressed with a 256
colour palette. Uncompressed textures either carry an alpha plane or not.
BLP2 adds DXT1, DXT3 and DXT5 block compression and plain BGRA pixels. A
file holds the full size image followed by its mipmaps, each half the size
of the previous one.

Decoding yields t'Image' t'PixelRGBA8' whatever the content, and reports
files it cannot read as a 'BlpDecodeError'. Files from the wild are often
slightly off spec; the decoder accepts the common deviations and, when a
mipmap is damaged, keeps the levels before it.

> Right img <- readBlp "war3mapMap.blp"
> writePng "war3mapMap.png" img
>
> Right png <- readImage "icon.png"
> result <- writeBlpWith "icon.blp" defaultBlpEncodeOptions png
-}
module Codec.Picture.Blp(
  -- * Decoding
  -- | Decoding allocates the full size image, whose size comes from the
  -- file. For untrusted input, check it first with 'blpInfo' or lower
  -- 'blpMaxPixels'.
    readBlp
  , readBlpMipmaps
  , decodeBlp
  , decodeBlpMipmaps
  , decodeBlpWith
  , decodeBlpMipmapsWith
  , BlpDecodeOptions(..)
  , defaultBlpDecodeOptions
  , BlpDecodeError(..)
  , displayBlpDecodeError
  -- * Inspecting
  , blpInfo
  , BlpInfo(..)
  , BlpVersion(..)
  , BlpCompression(..)
  -- * Encoding
  , encodeBlpWith
  , writeBlpWith
  , BlpEncodeOptions(..)
  , defaultBlpEncodeOptions
  , BlpFormat(..)
  , BlpEncodeError(..)
  , mipMapsUpTo
  -- * Deprecated encoders
  -- | These predate 'encodeBlpWith'. They clamp the JPEG quality to 1..100
  -- and throw on images 'encodeBlpWith' rejects.
  , writeBlpJpeg
  , writeBlpUncompressedWithAlpha
  , writeBlpUncompressedWithoutAlpha
  , encodeBlpJpeg
  , encodeBlpUncompressedWithAlpha
  , encodeBlpUncompressedWithoutAlpha
  ) where

import Codec.Picture
import Data.ByteString (ByteString)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Codec.Picture.Blp.Internal.Data
import Codec.Picture.Blp.Internal.Decoder
import Codec.Picture.Blp.Internal.Encoder
import Codec.Picture.Blp.Internal.Parser
import Codec.Picture.Blp.Types

-- | Read a BLP file and decode its full size image. IO errors, such as a
-- missing file, are thrown; malformed contents give 'Left'.
readBlp :: FilePath -> IO (Either BlpDecodeError (Image PixelRGBA8))
readBlp fp = decodeBlp `fmap` BS.readFile fp

-- | Read a BLP file and decode the full size image followed by its mipmaps.
-- See 'decodeBlpMipmaps'.
readBlpMipmaps :: FilePath -> IO (Either BlpDecodeError [Image PixelRGBA8])
readBlpMipmaps fp = decodeBlpMipmaps `fmap` BS.readFile fp

-- | Decode the full size image of a BLP file with 'defaultBlpDecodeOptions'.
decodeBlp :: ByteString -> Either BlpDecodeError (Image PixelRGBA8)
decodeBlp = decodeBlpWith defaultBlpDecodeOptions

-- | Decode the full size image of a BLP file.
decodeBlpWith :: BlpDecodeOptions -> ByteString -> Either BlpDecodeError (Image PixelRGBA8)
decodeBlpWith opts bs = do
  is <- decodeBlpMipmapsWith opts bs
  case is of
    i : _ -> Right i
    [] -> Left BlpNoImageData

-- | Decode the full size image followed by its mipmaps with
-- 'defaultBlpDecodeOptions'. See 'decodeBlpMipmapsWith'.
decodeBlpMipmaps :: ByteString -> Either BlpDecodeError [Image PixelRGBA8]
decodeBlpMipmaps = decodeBlpMipmapsWith defaultBlpDecodeOptions

-- | Decode the full size image followed by its mipmaps.
--
-- Element @i@ is always mip level @i@. Mipmaps are decoded lazily, and the
-- list stops at the first one that is damaged, so a broken mipmap never
-- costs the levels before it. A damaged full size image fails the file.
decodeBlpMipmapsWith :: BlpDecodeOptions -> ByteString -> Either BlpDecodeError [Image PixelRGBA8]
decodeBlpMipmapsWith opts bs = parseBlpFile bs >>= decodeBlpFile opts

-- | Read the header of a BLP file. This checks what 'decodeBlp' checks before
-- decoding (format, size, mipmap slots) but decodes no pixels, so it is cheap
-- on any input. Use it to reject textures that are too large.
blpInfo :: ByteString -> Either BlpDecodeError BlpInfo
blpInfo bs = blpFileInfo <$> parseBlpFile bs

-- | Number of mip levels, mip 0 included, whose larger side is at least the
-- given length; never less than one and at most 16. Use it as 'blpMipLevels'.
-- With 1 that is the full chain down to 1x1: five levels for 16x16, seven
-- for 64x16. With 4, a 16x16 texture keeps three levels.
mipMapsUpTo :: Int -- ^ Smallest side length to keep (2, 4, 16, ...)
  -> DynamicImage -- ^ Image the mipmaps are made for
  -> Int
mipMapsUpTo minSize img = max 1 $ mipLevelsDownTo minSize (dynamicMap imageWidth img) (dynamicMap imageHeight img)

-- | Encode an image as a BLP1 file.
encodeBlpWith :: BlpEncodeOptions -> DynamicImage -> Either BlpEncodeError BSL.ByteString
encodeBlpWith opts@BlpEncodeOptions{..} img
  | dynamicMap imageWidth img < 1 || dynamicMap imageHeight img < 1 = Left BlpEmptyImage
  | Just limit <- blpMaxSize, limit < 1 = Left $ BlpInvalidMaxSize limit
  | w > 65535 || h > 65535 = Left $ BlpImageTooLarge w h
  | BlpFormatJpeg quality <- blpFormat, quality < 1 || quality > 100 = Left $ BlpInvalidJpegQuality quality
  | BSL.length file > 0xFFFFFFFF = Left BlpFileTooLarge
  | otherwise = Right file
  where
    source = maybe img (`shrinkTo` img) blpMaxSize
    w = dynamicMap imageWidth source
    h = dynamicMap imageHeight source
    file = encodeBlp (toBlpStruct opts source)

-- | Encode an image with 'encodeBlpWith' and write it to a file.
writeBlpWith :: FilePath -> BlpEncodeOptions -> DynamicImage -> IO (Either BlpEncodeError ())
writeBlpWith fp opts img = traverse (BSL.writeFile fp) (encodeBlpWith opts img)

-- | Encode with 'encodeBlpJpeg' and write the result to a file.
writeBlpJpeg :: FilePath -> Int -> Int -> DynamicImage -> IO ()
writeBlpJpeg fp quality numMips img = BSL.writeFile fp $ encodeBlpJpeg quality numMips img
{-# DEPRECATED writeBlpJpeg "Use writeBlpWith" #-}

-- | Encode with 'encodeBlpUncompressedWithAlpha' and write the result to a file.
writeBlpUncompressedWithAlpha :: FilePath -> Int -> DynamicImage -> IO ()
writeBlpUncompressedWithAlpha fp numMips img = BSL.writeFile fp $ encodeBlpUncompressedWithAlpha numMips img
{-# DEPRECATED writeBlpUncompressedWithAlpha "Use writeBlpWith" #-}

-- | Encode with 'encodeBlpUncompressedWithoutAlpha' and write the result to a
-- file.
writeBlpUncompressedWithoutAlpha :: FilePath -> Int -> DynamicImage -> IO ()
writeBlpUncompressedWithoutAlpha fp numMips img = BSL.writeFile fp $ encodeBlpUncompressedWithoutAlpha numMips img
{-# DEPRECATED writeBlpUncompressedWithoutAlpha "Use writeBlpWith" #-}

-- | Encode as a JPEG compressed BLP; see 'BlpFormatJpeg'.
encodeBlpJpeg :: Int -- ^ JPEG quality, clamped to 1..100
  -> Int -- ^ Number of mip levels to write
  -> DynamicImage
  -> BSL.ByteString
encodeBlpJpeg quality numMips =
  orThrow "encodeBlpJpeg" . encodeBlpWith defaultBlpEncodeOptions { blpFormat = BlpFormatJpeg (max 1 (min 100 quality)), blpMipLevels = numMips }
{-# DEPRECATED encodeBlpJpeg "Use encodeBlpWith with BlpFormatJpeg" #-}

-- | Encode with a 256 colour palette and 8-bit alpha; see 'BlpFormatPaletted'.
encodeBlpUncompressedWithAlpha :: Int -- ^ Number of mip levels to write
  -> DynamicImage
  -> BSL.ByteString
encodeBlpUncompressedWithAlpha numMips =
  orThrow "encodeBlpUncompressedWithAlpha" . encodeBlpWith defaultBlpEncodeOptions { blpFormat = BlpFormatPaletted True, blpMipLevels = numMips }
{-# DEPRECATED encodeBlpUncompressedWithAlpha "Use encodeBlpWith with BlpFormatPaletted True" #-}

-- | Encode with a 256 colour palette and no alpha; see 'BlpFormatPaletted'.
encodeBlpUncompressedWithoutAlpha :: Int -- ^ Number of mip levels to write
  -> DynamicImage
  -> BSL.ByteString
encodeBlpUncompressedWithoutAlpha numMips =
  orThrow "encodeBlpUncompressedWithoutAlpha" . encodeBlpWith defaultBlpEncodeOptions { blpFormat = BlpFormatPaletted False, blpMipLevels = numMips }
{-# DEPRECATED encodeBlpUncompressedWithoutAlpha "Use encodeBlpWith with BlpFormatPaletted False" #-}

orThrow :: String -> Either BlpEncodeError a -> a
orThrow name = either (\e -> error $ name ++ ": " ++ show e) id
