module Codec.Picture.Blp(

  ) where

import Data.ByteString (ByteString)
import Data.Vector.Unboxed (Vector)
import qualified Data.ByteString as B 
import qualified Data.Vector.Unboxed as V 
import Data.Word 

data BlpStruct = BlpStruct {
  blpCompression :: !BlpCompression 
, blpFlags :: ![BlpFlag]
, blpWidth :: !Int 
, blpHeight :: !Int
, blpPictureType :: !BlpPictureType  
, blpPictureSubType :: !Word32 -- is not used
, blpMipMapOffset :: ![Word32] -- length is always 16
, blpMipMapSize :: ![Word32] -- length is always 16
, blpExt :: BlpExt 
}

data BlpExt = 
    BlpJpeg {
      blpJpegHeader :: !ByteString
    , blpJpegData :: ![ByteString]
    }
  | BlpUncompressed1 {
      blpU1Palette :: !(Vector Word32)
    , blpU1MipMaps :: ![(ByteString, ByteString)]
    }
  | BlpUncompressed2 {
      blpU2Palette :: !(Vector Word32)
    , blpU2MipMaps :: ![ByteString]
    }

data BlpCompression =
    BlpCompressionJPEG
  | BlpCompressionUncompressed
  deriving (Eq, Ord, Enum, Bounded, Show)

data BlpFlag = BlpFlagAlphaChannel
  deriving (Eq, Ord, Enum, Bounded, Show)

data BlpPictureType = 
    UncompressedWithAlpha
  | UncompressedWithoutAlpha
  | JpegPicture