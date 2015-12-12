module Codec.Picture.Blp(

  ) where

import Control.Monad
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Data.Vector.Unboxed (Vector)
import Data.Word 
import qualified Data.ByteString as B 
import qualified Data.Vector.Unboxed as V 

data BlpStruct = BlpStruct {
  blpCompression :: !BlpCompression 
, blpFlags :: ![BlpFlag]
, blpWidth :: !Word32 
, blpHeight :: !Word32
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

blpParser :: Parser BlpStruct
blpParser = do 
  guardBlpVersion
  blpCompression <- compressionParser
  blpFlags <- flagsParser
  blpWidth <- dword <?> "width"
  blpHeight <- dword <?> "height"
  blpPictureType <- pictureTypeParser
  blpPictureSubType <- dword <?> "picture subtype"
  blpMipMapOffset <- replicateM 16 dword <?> "mipmaps offsets"
  blpMipMapSize <- replicateM 16 dword <?> "mipmaps sizes"
  blpExt <- case blpCompression of 
    BlpCompressionJPEG -> blpJpegParser
    BlpCompressionUncompressed -> case blpPictureType of 
      UncompressedWithAlpha -> blpUncompressed1Parser
      UncompressedWithoutAlpha -> blpUncompressed2Parser

  return $ BlpStruct {..} 

guardBlpVersion :: Parser ()
guardBlpVersion = do 
  ver <- replicateM 4 anyChar
  unless (ver == "BLP1") $ fail "File is not in BLP1 format!"

dword :: Parser Word32
dword = undefined

compressionParser :: Parser BlpCompression
compressionParser = undefined

flagsParser :: Parser [BlpFlag]
flagsParser = undefined

pictureTypeParser :: Parser BlpPictureType 
pictureTypeParser = undefined

blpJpegParser :: Parser BlpExt 
blpJpegParser = undefined

blpUncompressed1Parser :: Parser BlpExt
blpUncompressed1Parser = undefined

blpUncompressed2Parser :: Parser BlpExt
blpUncompressed2Parser = undefined