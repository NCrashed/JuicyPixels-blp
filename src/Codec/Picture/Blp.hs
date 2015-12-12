module Codec.Picture.Blp(

  ) where

import Control.Monad
import Data.Attoparsec.Binary
import Data.Attoparsec.ByteString.Char8 as AT
import Data.Bits
import Data.ByteString (ByteString)
import Data.Vector.Unboxed (Vector)
import Data.Word 
import qualified Data.ByteString as B 
import qualified Data.Vector.Unboxed as V 
import qualified Data.Attoparsec.Internal.Types as AT 

data BlpStruct = BlpStruct {
  blpCompression :: !BlpCompression 
, blpFlags :: ![BlpFlag]
, blpWidth :: !Word32 
, blpHeight :: !Word32
, blpPictureType :: !BlpPictureType  
, blpPictureSubType :: !Word32 -- is not used
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
  let mipMapsInfo = filter ((>0).snd) $ blpMipMapOffset `zip` blpMipMapSize
  blpExt <- case blpCompression of 
    BlpCompressionJPEG -> blpJpegParser mipMapsInfo
    BlpCompressionUncompressed -> case blpPictureType of 
      UncompressedWithAlpha -> blpUncompressed1Parser mipMapsInfo
      UncompressedWithoutAlpha -> blpUncompressed2Parser mipMapsInfo

  return $ BlpStruct {..} 

guardBlpVersion :: Parser ()
guardBlpVersion = do 
  ver <- replicateM 4 anyChar
  unless (ver == "BLP1") $ fail "File is not in BLP1 format!"

dword :: Parser Word32
dword = anyWord32le

compressionParser :: Parser BlpCompression
compressionParser = (<?> "compression") $ do 
  i <- dword
  case i of 
    0 -> return BlpCompressionJPEG
    1 -> return BlpCompressionUncompressed
    _ -> fail "Unknown compression"

flagsParser :: Parser [BlpFlag]
flagsParser = (<?> "flags") $ do 
  i <- dword
  return $ if i `testBit` 3
    then [BlpFlagAlphaChannel]
    else []

pictureTypeParser :: Parser BlpPictureType 
pictureTypeParser = (<?> "picture type") $ do 
  i <- dword
  case i of 
    3 -> return UncompressedWithAlpha
    4 -> return UncompressedWithAlpha
    5 -> return UncompressedWithoutAlpha
    _ -> fail "Unknown picture type"

blpJpegParser :: [(Word32, Word32)] -> Parser BlpExt 
blpJpegParser mps = (<?> "blp jpeg") $ do 
  headerSize <- dword <?> "jpeg header size"
  blpJpegHeader <- AT.take (fromIntegral headerSize) <?> "jpeg header"
  blpJpegData <- forM mps $ \(offset, size) -> do 
    skipToOffset offset 
    AT.take $ fromIntegral size
  return $ BlpJpeg {..}

getPos :: Parser Int
getPos = AT.Parser $ \t pos more _ succ' -> succ' t pos more (AT.fromPos pos)

skipToOffset :: Word32 -> Parser ()
skipToOffset i = do 
  pos <- getPos
  let diff = fromIntegral i - pos
  if diff <= 0 then return ()
    else void $ AT.take diff

blpUncompressed1Parser :: [(Word32, Word32)] -> Parser BlpExt
blpUncompressed1Parser mps = do 
  blpU1Palette <- V.replicateM 256 dword
  blpU1MipMaps <- forM mps $ \(offset, size) -> do 
    skipToOffset offset 
    let halfSize = fromIntegral size `div` 2
    indexList <- AT.take halfSize <?> "index list"
    alphaList <- AT.take halfSize <?> "alpha list"
    return (indexList, alphaList)
  return $ BlpUncompressed1 {..}

blpUncompressed2Parser :: [(Word32, Word32)] -> Parser BlpExt
blpUncompressed2Parser mps = do 
  blpU2Palette <- V.replicateM 256 dword
  blpU2MipMaps <- forM mps $ \(offset, size) -> do 
    skipToOffset offset 
    let halfSize = fromIntegral size `div` 2
    AT.take halfSize <?> "index list"
  return $ BlpUncompressed2 {..}