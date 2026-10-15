{-|
The structure of a BLP1 file as the parser reads it and the encoder writes
it. See @docs/format.md@ in the source repository for the full format.

This is an internal module: its API may change between minor versions.
-}
module Codec.Picture.Blp.Internal.Data(
    BlpStruct(..)
  , blpCompression
  , BlpCompression(..)
  , BlpExt(..)
  , BlpFormat(..)
  , BlpEncodeOptions(..)
  , BlpEncodeError(..)
  ) where

import Codec.Picture
import Data.ByteString (ByteString)
import Data.Vector (Vector)
import Data.Word
import GHC.Generics

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

-- | Header dword @0x04@.
data BlpCompression =
    BlpCompressionJPEG -- ^ 0: JPEG
  | BlpCompressionUncompressed -- ^ 1: 256 colour palette
  deriving (Eq, Ord, Enum, Bounded, Show, Generic)

-- | Header dword @0x04@ (@content@ in the specification) of a file.
blpCompression :: BlpStruct -> BlpCompression
blpCompression blp = case blpExt blp of
  BlpJpeg {} -> BlpCompressionJPEG
  BlpPaletted {} -> BlpCompressionUncompressed

-- | How to encode a texture.
data BlpFormat =
    -- | JPEG with the given quality, from 1 to 100. The texture gets alpha
    -- when the image has an alpha channel.
    BlpFormatJpeg !Int
    -- | 256 colour palette, with 8-bit alpha or without alpha. Colours are
    -- quantised with dithering.
  | BlpFormatPaletted !Bool
  deriving (Eq, Show)

-- | Options for encoding a texture.
data BlpEncodeOptions = BlpEncodeOptions {
  -- | Content type and its settings.
  blpFormat :: !BlpFormat
  -- | Number of mip levels to write, mip 0 included. Values below 1 mean 1,
  -- and values above what the image has mean the full chain down to 1x1
  -- (at most 16 levels).
, blpMipLevels :: !Int
} deriving (Eq, Show)

-- | Why an image cannot be encoded.
data BlpEncodeError =
    BlpEmptyImage -- ^ The image has a side of zero pixels.
  | BlpImageTooLarge !Int !Int -- ^ Width or height is above 65535, the format's limit.
  | BlpInvalidJpegQuality !Int -- ^ JPEG quality outside 1 to 100.
  | BlpFileTooLarge -- ^ The file would exceed 4 GiB, the most its offsets can address.
  deriving (Eq, Show)
