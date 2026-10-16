{-|
The structure of BLP1 and BLP2 files as the parser reads them, and of BLP1
files as the encoder writes them. See @docs/format.md@ in the source
repository for the full format.

'BlpCompression', 'BlpFormat', t'BlpEncodeOptions' and 'BlpEncodeError' are
re-exported from "Codec.Picture.Blp", where they belong to the stable API.

This is an internal module: its API may change between minor versions.
-}
module Codec.Picture.Blp.Internal.Data(
    BlpStruct(..)
  , blpCompression
  , mipChainLength
  , mipLevelsDownTo
  , BlpCompression(..)
  , BlpExt(..)
  , Blp2Struct(..)
  , blp2Compression
  , Blp2Content(..)
  , BlpFile(..)
  , BlpFormat(..)
  , BlpEncodeOptions(..)
  , BlpEncodeError(..)
  ) where

import Codec.Picture
import Data.ByteString (ByteString)
import Data.Vector (Vector)
import Data.Word
import GHC.Generics

import Codec.Picture.Blp.Internal.Dxt (DxtFormat(..))
import Codec.Picture.Blp.Types

-- | A parsed BLP1 file: the 156 byte header and the image data after it. The
-- content type (header dword @0x04@) follows from 'blpExt', see
-- 'blpCompression'.
data BlpStruct = BlpStruct {
  -- | Header dword @0x08@: alpha bits per pixel. 0, 1, 4 and 8 are valid for
  -- paletted content, 0 and 8 for JPEG; any other value means no alpha.
  blpAlphaBits :: !Word32
  -- | Width of mip 0 in pixels.
, blpWidth :: !Word32
  -- | Height of mip 0 in pixels.
, blpHeight :: !Word32
  -- | Header dword @0x14@. It has no known effect and is kept as read.
, blpExtra :: !Word32
  -- | Header dword @0x18@: zero means only mip 0 is present.
, blpHasMipmaps :: !Word32
  -- | Everything after the header, including the mip levels.
, blpExt :: BlpExt
} deriving (Show, Generic)

-- | The part of the file after the header.
data BlpExt =
    -- | JPEG content. Each mip level is @blpJpegHeader <> level@, a
    -- four-component JPEG whose components are B, G, R and A.
    BlpJpeg {
      blpJpegHeader :: !ByteString -- ^ Shared JPEG header, at most 624 bytes
    , blpJpegData :: ![ByteString] -- ^ Rest of each mip level, mip 0 first
    }
    -- | Paletted content. Each mip level holds the index list followed by the
    -- alpha list, packed at 'blpAlphaBits' bits per pixel; the level's size
    -- tells where one ends and the other begins.
  | BlpPaletted {
      blpPalette :: !(Vector PixelRGBA8) -- ^ 256 entries stored as B, G, R and padding
    , blpPalettedMipMaps :: ![ByteString] -- ^ Raw data of each mip level, mip 0 first
    }
  deriving (Show, Generic)

-- | A parsed BLP2 file, the World of Warcraft variant: the 148 byte header,
-- the palette and the raw mip levels.
data Blp2Struct = Blp2Struct {
  -- | Header byte @0x09@: alpha bits per pixel. 0 means the image is
  -- opaque, whatever the content holds.
  blp2AlphaDepth :: !Word8
  -- | Header byte @0x0A@: which DXT format, for DXT content (0, 1 or 7);
  -- kept as read.
, blp2AlphaType :: !Word8
  -- | Header byte @0x0B@: zero means only mip 0 is present. Kept as read;
  -- the slots decide which levels are read, as in BLP1.
, blp2HasMips :: !Word8
  -- | Width of mip 0 in pixels.
, blp2Width :: !Word32
  -- | Height of mip 0 in pixels.
, blp2Height :: !Word32
  -- | How the levels are stored, from header bytes @0x08@ and @0x0A@.
, blp2Content :: !Blp2Content
  -- | Raw data of each mip level, mip 0 first.
, blp2MipMaps :: ![ByteString]
} deriving (Show, Generic)

-- | The content of a BLP2 file.
data Blp2Content =
    -- | Encoding 1: palette indices then alpha, as in paletted BLP1. 256
    -- entries stored as B, G, R and padding.
    Blp2Paletted !(Vector PixelRGBA8)
    -- | Encoding 2: DXT blocks.
  | Blp2Dxt !DxtFormat
    -- | Encoding 3: four bytes per pixel, B, G, R and A.
  | Blp2Bgra
  deriving (Show, Generic)

-- | How a BLP2 file stores its pixels.
blp2Compression :: Blp2Struct -> BlpCompression
blp2Compression blp = case blp2Content blp of
  Blp2Paletted _ -> BlpCompressionUncompressed
  Blp2Dxt (Dxt1 _) -> BlpCompressionDxt1
  Blp2Dxt Dxt3 -> BlpCompressionDxt3
  Blp2Dxt Dxt5 -> BlpCompressionDxt5
  Blp2Bgra -> BlpCompressionBgra

-- | A parsed BLP1 or BLP2 file.
data BlpFile = BlpFile1 !BlpStruct | BlpFile2 !Blp2Struct
  deriving (Show, Generic)

-- | Header dword @0x04@ (@content@ in the specification) of a file.
blpCompression :: BlpStruct -> BlpCompression
blpCompression blp = case blpExt blp of
  BlpJpeg {} -> BlpCompressionJPEG
  BlpPaletted {} -> BlpCompressionUncompressed


-- | Number of levels in a full mip chain: halving the larger side down to 1,
-- at most 16.
mipChainLength :: Int -> Int -> Int
mipChainLength = mipLevelsDownTo 1

-- | Number of mip levels of a @w@ x @h@ texture, mip 0 included, whose
-- larger side is at least the given length (taken as 1 when smaller), at
-- most 16. Zero when the texture itself is smaller.
mipLevelsDownTo :: Int -> Int -> Int -> Int
mipLevelsDownTo minSide w h = min 16 . length . takeWhile (>= max 1 minSide) $ iterate (`div` 2) (max w h)
