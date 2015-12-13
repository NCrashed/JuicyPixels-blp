module Codec.Picture.Blp.Internal.Data(
    BlpStruct(..)
  , BlpCompression(..)
  , BlpFlag(..)
  , BlpPictureType(..)
  , BlpExt(..)
  ) where

import Codec.Picture
import Data.ByteString (ByteString)
import Data.Hashable
import Data.Vector (Vector)
import Data.Word 
import GHC.Generics 

data BlpStruct = BlpStruct {
  blpCompression :: !BlpCompression 
, blpFlags :: ![BlpFlag]
, blpWidth :: !Word32 
, blpHeight :: !Word32
, blpPictureType :: !BlpPictureType  
, blpPictureSubType :: !Word32 -- is not used
, blpExt :: BlpExt 
} deriving (Show, Generic)

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

data BlpCompression =
    BlpCompressionJPEG
  | BlpCompressionUncompressed
  deriving (Eq, Ord, Enum, Bounded, Show, Generic)

instance Hashable BlpCompression

data BlpFlag = BlpFlagAlphaChannel
  deriving (Eq, Ord, Enum, Bounded, Show, Generic)

instance Hashable BlpFlag 

data BlpPictureType = 
    JPEGType
  | UncompressedWithAlpha
  | UncompressedWithoutAlpha
  deriving (Eq, Ord, Enum, Bounded, Show, Generic)

instance Hashable BlpPictureType