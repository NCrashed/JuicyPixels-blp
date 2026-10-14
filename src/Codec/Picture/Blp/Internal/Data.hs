{-|
The structure of a BLP1 file as the parser reads it and the encoder writes
it. See @docs/format.md@ in the source repository for the full format.

Some field names follow an older description of the format and do not match
what the fields mean; each field's documentation gives its real meaning.

This is an internal module: its API may change between minor versions.
-}
module Codec.Picture.Blp.Internal.Data(
    BlpStruct(..)
  , BlpCompression(..)
  , BlpFlag(..)
  , BlpPictureType(..)
  , BlpExt(..)
  ) where

import Codec.Picture
import Data.ByteString (ByteString)
import Data.Vector (Vector)
import Data.Word
import GHC.Generics

-- | A parsed BLP1 file: the 156 byte header and the image data after it.
data BlpStruct = BlpStruct {
  -- | Header dword @0x04@, called @content@ in the specification.
  blpCompression :: !BlpCompression
  -- | Header dword @0x08@, @alphaBits@ in the specification: 0, 1, 4 or 8.
  -- Only the value 8 is recognised, as 'BlpFlagAlphaChannel'.
, blpFlags :: ![BlpFlag]
  -- | Width of mip 0 in pixels.
, blpWidth :: !Word32
  -- | Height of mip 0 in pixels.
, blpHeight :: !Word32
  -- | Header dword @0x14@, @extra@ in the specification. It has no known
  -- effect, but the decoder uses it to tell paletted files with and without
  -- alpha apart.
, blpPictureType :: !BlpPictureType
  -- | Header dword @0x18@, @hasMipmaps@ in the specification: zero means only
  -- mip 0 is present. The encoder writes 5.
, blpPictureSubType :: !Word32
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
    -- | Paletted content with 8-bit alpha.
  | BlpUncompressed1 {
      blpU1Palette :: !(Vector PixelRGBA8) -- ^ 256 entries stored as B, G, R and padding
    , blpU1MipMaps :: ![(ByteString, ByteString)] -- ^ Index list and alpha list of each mip level
    }
    -- | Paletted content without alpha.
  | BlpUncompressed2 {
      blpU2Palette :: !(Vector PixelRGBA8) -- ^ 256 entries stored as B, G, R and padding
    , blpU2MipMaps :: ![ByteString] -- ^ Index list of each mip level
    }
  deriving (Show, Generic)

-- | Header dword @0x04@.
data BlpCompression =
    BlpCompressionJPEG -- ^ 0: JPEG
  | BlpCompressionUncompressed -- ^ 1: 256 colour palette
  deriving (Eq, Ord, Enum, Bounded, Show, Generic)

-- | Values of header dword @0x08@ (@alphaBits@) that the library recognises.
data BlpFlag =
    BlpFlagAlphaChannel -- ^ 8: 8-bit alpha
  deriving (Eq, Ord, Enum, Bounded, Show, Generic)

-- | How the library reads header dword @0x14@ (@extra@).
data BlpPictureType =
    JPEGType -- ^ 2 or 6. Accepted on JPEG files only.
  | UncompressedWithAlpha -- ^ 0, 3 or 4. Paletted with 8-bit alpha; the encoder writes 3.
  | UncompressedWithoutAlpha -- ^ 5. Paletted without alpha; the encoder writes it.
  deriving (Eq, Ord, Enum, Bounded, Show, Generic)
