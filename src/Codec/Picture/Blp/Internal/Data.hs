module Codec.Picture.Blp.Internal.Data(
    BlpStruct(..)
  , BlpCompression(..)
  , BlpFlag(..)
  , BlpPictureType(..)
  , BlpExt(..)
  ) where

import Codec.Picture
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Hashable
import Data.Monoid
import Data.Vector (Vector)
import Data.Word
import GHC.Generics
import TextShow

import qualified Data.ByteString as BS
import qualified Data.Vector as V

data BlpStruct = BlpStruct {
  blpCompression :: !BlpCompression
, blpFlags :: ![BlpFlag]
, blpWidth :: !Word32
, blpHeight :: !Word32
, blpPictureType :: !BlpPictureType
, blpPictureSubType :: !Word32 -- is not used
, blpExt :: BlpExt
} deriving (Show, Generic)

instance TextShow BlpStruct where
  showb BlpStruct{..} = "BlpStruct {\n"
    <> "  blpCompression = " <> showb blpCompression <> "\n"
    <> ", blpFlags = " <> showb blpFlags <> "\n"
    <> ", blpWidth = " <> showb blpWidth <> "\n"
    <> ", blpHeight = " <> showb blpHeight <> "\n"
    <> ", blpPictureType = " <> showb blpPictureType <> "\n"
    <> ", blpPictureSubType = " <> showb blpPictureSubType <> "\n"
    <> ", blpExt = " <> showb blpExt <> "\n"
    <> "}"

data BlpExt =
    BlpJpeg {
      blpJpegHeader :: !ByteString
    , blpJpegData :: ![ByteString]
    }
  | BlpUncompressed1 {
      blpU1Palette :: !(Vector PixelRGBA8)
    , blpU1MipMaps :: ![(ByteString, ByteString)]
    }
  | BlpUncompressed2 {
      blpU2Palette :: !(Vector PixelRGBA8)
    , blpU2MipMaps :: ![ByteString]
    }
  deriving (Show, Generic)

-- | Helper to display bytestring as placeholder with length
showBinary :: ByteString -> Builder
showBinary v = "<bslength " <> tl v <> ">"
  where
    tl = showb . BS.length

-- | Helper to display bytestring as placeholder with length
showVector :: Vector a -> Builder
showVector v = "<veclength " <> tl v <> ">"
  where
    tl = showb . V.length

-- | Helper that works as Data.Text.intercalate
intercalateb :: Builder -> [Builder] -> Builder
intercalateb _ [] = mempty
intercalateb _ [x] = x 
intercalateb spacer (x:xs) = x <> spacer <> intercalateb spacer xs

instance TextShow BlpExt where
  showb v = case v of
    BlpJpeg{..} -> "BlpJpeg { \n"
      <> "  blpJpegHeader = " <> showBinary blpJpegHeader <> "\n"
      <> ", blpJpegData = [" <> intercalateb ", " (fmap showBinary blpJpegData) <> "]\n"
      <> "}"
    BlpUncompressed1{..} -> "BlpUncompressed1 { \n"
      <> "  blpU1Palette = " <> showVector blpU1Palette <> "\n"
      <> ", blpU1MipMaps = [" <> intercalateb ", " ( (\(a, b) -> "(" <> showBinary a <> ", " <> showBinary b <> ")") <$> blpU1MipMaps) <> "]\n"
      <> "}"
    BlpUncompressed2{..} -> "BlpUncompressed2 { \n"
      <> "  blpU2Palette = " <> showVector blpU2Palette <> "\n"
      <> ", blpU2MipMaps = [" <> intercalateb ", " (fmap showBinary blpU2MipMaps) <> "]\n"
      <> "}"

data BlpCompression =
    BlpCompressionJPEG
  | BlpCompressionUncompressed
  deriving (Eq, Ord, Enum, Bounded, Show, Generic)

instance TextShow BlpCompression where
  showb v = case v of
    BlpCompressionJPEG -> "BlpCompressionJPEG"
    BlpCompressionUncompressed -> "BlpCompressionUncompressed"

instance Hashable BlpCompression

data BlpFlag = BlpFlagAlphaChannel
  deriving (Eq, Ord, Enum, Bounded, Show, Generic)

instance TextShow BlpFlag where
  showb v = case v of
    BlpFlagAlphaChannel -> "BlpFlagAlphaChannel"

instance Hashable BlpFlag

data BlpPictureType =
    JPEGType
  | UncompressedWithAlpha
  | UncompressedWithoutAlpha
  deriving (Eq, Ord, Enum, Bounded, Show, Generic)

instance TextShow BlpPictureType where
  showb v = case v of
    JPEGType -> "JPEGType"
    UncompressedWithAlpha -> "UncompressedWithAlpha"
    UncompressedWithoutAlpha -> "UncompressedWithoutAlpha"

instance Hashable BlpPictureType
