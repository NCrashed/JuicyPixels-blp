{-|
The option, result and error types of the public API. They are re-exported
from "Codec.Picture.Blp", and the ones the internal modules use from
"Codec.Picture.Blp.Internal.Data" as well; this module is not exposed.
-}
module Codec.Picture.Blp.Types(
    BlpDecodeOptions(..)
  , defaultBlpDecodeOptions
  , BlpDecodeError(..)
  , displayBlpDecodeError
  , BlpInfo(..)
  , BlpVersion(..)
  , BlpCompression(..)
  , BlpFormat(..)
  , BlpEncodeOptions(..)
  , defaultBlpEncodeOptions
  , BlpEncodeError(..)
  ) where

import Control.DeepSeq (NFData)
import Control.Exception (Exception(..))
import GHC.Generics

-- | Options for decoding.
newtype BlpDecodeOptions = BlpDecodeOptions {
  -- | Largest full size image to decode, in pixels. Larger files are
  -- rejected before anything is allocated; decoding takes about four bytes
  -- per pixel.
  blpMaxPixels :: Int
} deriving (Eq, Show)

-- | Up to 8192x8192 pixels, about 256 MiB for the full size image.
defaultBlpDecodeOptions :: BlpDecodeOptions
defaultBlpDecodeOptions = BlpDecodeOptions { blpMaxPixels = 8192 * 8192 }

-- | Why a BLP file cannot be decoded. 'displayBlpDecodeError' gives a
-- readable message.
data BlpDecodeError =
    -- | The file starts with neither @BLP1@ nor @BLP2@. BLP0 files are among
    -- these.
    BlpNotBlp
    -- | The file ends inside its header.
  | BlpTruncatedHeader
    -- | The header asks for something this library does not read, such as
    -- an unknown content type or BLP2 with JPEG content. The text says what.
  | BlpUnsupported String
    -- | Width or height outside 1 to 65535.
  | BlpInvalidSize !Int !Int
    -- | Width and height whose product is above 'blpMaxPixels', the limit
    -- given last.
  | BlpTooLarge !Int !Int !Int
    -- | No mipmap slot points at usable data.
  | BlpNoImageData
    -- | The full size image cannot be decoded: its data is too short, its
    -- JPEG stream is broken, and so on. The text says what. Damaged
    -- mipmaps after the first are not errors; the list of levels stops
    -- before them.
  | BlpCorruptImage String
  deriving (Eq, Show, Generic)

instance Exception BlpDecodeError where
  displayException = displayBlpDecodeError

-- | A message for an error, in plain words.
displayBlpDecodeError :: BlpDecodeError -> String
displayBlpDecodeError e = case e of
  BlpNotBlp -> "Not a BLP file: it starts with neither BLP1 nor BLP2"
  BlpTruncatedHeader -> "The file ends inside its header"
  BlpUnsupported what -> "Unsupported BLP file: " ++ what
  BlpInvalidSize w h -> "Invalid image size " ++ show w ++ "x" ++ show h
  BlpTooLarge w h limit -> "Image size " ++ show w ++ "x" ++ show h
    ++ " is above the limit of " ++ show limit ++ " pixels"
  BlpNoImageData -> "The file has no image data"
  BlpCorruptImage what -> "The full size image is damaged: " ++ what

-- | What a BLP file holds, read from its header without decoding any pixels.
data BlpInfo = BlpInfo {
    blpInfoVersion :: !BlpVersion -- ^ BLP1 or BLP2
  , blpInfoCompression :: !BlpCompression -- ^ How the pixels are stored
  , blpInfoWidth :: !Int -- ^ Width of the full size image
  , blpInfoHeight :: !Int -- ^ Height of the full size image
  , blpInfoAlphaBits :: !Int -- ^ The alpha depth header field (@alphaBits@ in BLP1, @alphaDepth@ in BLP2), as stored
  , blpInfoMipLevels :: !Int -- ^ Mip levels with usable slots, the full size image included
  } deriving (Eq, Show, Generic)

-- | Version of a BLP file: BLP1 is used by Warcraft III, BLP2 by World of
-- Warcraft.
data BlpVersion = Blp1 | Blp2
  deriving (Eq, Ord, Enum, Bounded, Show, Generic)

-- | How the pixels of a BLP file are stored.
data BlpCompression =
    BlpCompressionJPEG -- ^ JPEG (BLP1 only)
  | BlpCompressionUncompressed -- ^ 256 colour palette
  | BlpCompressionDxt1 -- ^ DXT1 blocks (BLP2 only)
  | BlpCompressionDxt3 -- ^ DXT3 blocks (BLP2 only)
  | BlpCompressionDxt5 -- ^ DXT5 blocks (BLP2 only)
  | BlpCompressionBgra -- ^ 32-bit BGRA pixels (BLP2 only)
  deriving (Eq, Ord, Enum, Bounded, Show, Generic)

-- | How to encode a texture. JPEG is BLP1 only; DXT and BGRA are BLP2 only;
-- a palette works in both, see 'blpVersion'.
data BlpFormat =
    -- | JPEG with the given quality, from 1 to 100. The texture gets alpha
    -- when the image has an alpha channel.
    BlpFormatJpeg !Int
    -- | 256 colour palette, with 8-bit alpha or without alpha. Colours are
    -- quantised with dithering.
  | BlpFormatPaletted !Bool
    -- | DXT1 blocks, 4 bits per pixel. The texture gets 1-bit alpha when
    -- some pixel's alpha is below 128; those pixels become transparent.
  | BlpFormatDxt1
    -- | DXT3 blocks: DXT1 colour plus 4-bit alpha per pixel.
  | BlpFormatDxt3
    -- | DXT5 blocks: DXT1 colour plus interpolated 8-bit alpha, smoother
    -- than DXT3 for gradual alpha.
  | BlpFormatDxt5
    -- | Uncompressed 32-bit pixels, lossless and 4 bytes per pixel.
  | BlpFormatBgra
  deriving (Eq, Show, Generic)

-- | Options for encoding a texture.
data BlpEncodeOptions = BlpEncodeOptions {
  -- | Content type and its settings.
  blpFormat :: !BlpFormat
  -- | Number of mip levels to write, mip 0 included. Values below 1 mean 1,
  -- and values above what the image has mean the full chain down to 1x1
  -- (at most 16 levels).
, blpMipLevels :: !Int
  -- | Whether paletted textures are dithered. Dithering makes gradients
  -- smoother and spoils icons and pixel art.
, blpDither :: !Bool
  -- | Shrink the image, halving both sides, until the larger side is at most
  -- this many pixels. Warcraft III shows mip levels up to 512 pixels only.
, blpMaxSize :: !(Maybe Int)
  -- | File version to write: 'Blp1' for Warcraft III, 'Blp2' for World of
  -- Warcraft. The format must exist in that version, see 'BlpFormat'.
, blpVersion :: !BlpVersion
} deriving (Eq, Show)

-- | BLP1, JPEG at quality 90 with the full mip chain, dithered palettes, and
-- no size limit.
defaultBlpEncodeOptions :: BlpEncodeOptions
defaultBlpEncodeOptions = BlpEncodeOptions {
    blpFormat = BlpFormatJpeg 90
  , blpMipLevels = 16
  , blpDither = True
  , blpMaxSize = Nothing
  , blpVersion = Blp1
  }

-- | Why an image cannot be encoded.
data BlpEncodeError =
    BlpEmptyImage -- ^ The image has a side of zero pixels.
  | BlpImageTooLarge !Int !Int -- ^ Width or height is above 65535, the format's limit.
  | BlpInvalidJpegQuality !Int -- ^ JPEG quality outside 1 to 100.
  | BlpFileTooLarge -- ^ The file would exceed 4 GiB, the most its offsets can address.
  | BlpInvalidMaxSize !Int -- ^ A 'blpMaxSize' below 1.
  | BlpUnsupportedFormat !BlpVersion !BlpFormat -- ^ The format does not exist in this version, such as JPEG in BLP2.
  deriving (Eq, Show, Generic)

instance Exception BlpEncodeError

instance NFData BlpDecodeError
instance NFData BlpEncodeError
instance NFData BlpInfo
instance NFData BlpVersion
instance NFData BlpCompression
instance NFData BlpFormat
