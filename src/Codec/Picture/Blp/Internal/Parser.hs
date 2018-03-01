module Codec.Picture.Blp.Internal.Parser(
    blpParser
  , blpVersion
  , dword
  , compressionParser
  , flagsParser
  , pictureTypeParser
  , blpJpegParser
  , getPos
  , skipToOffset
  , blpUncompressed1Parser
  , blpUncompressed2Parser
  , parseBlp
  ) where

import Codec.Picture
import Control.Monad
import Data.Attoparsec.ByteString as AT
import Data.Bits
import Data.ByteString (ByteString)
import Data.List (nub)
import Data.Word

import qualified Data.Vector as V
import qualified Data.Attoparsec.Internal.Types as AT
import qualified Data.ByteString as BS

import Codec.Picture.Blp.Internal.Data

blpParser :: Parser BlpStruct
blpParser = do
  _ <- blpVersion
  blpCompression <- compressionParser
  blpFlags <- flagsParser
  blpWidth <- dword <?> "width"
  blpHeight <- dword <?> "height"
  blpPictureType <- pictureTypeParser
  blpPictureSubType <- dword <?> "picture subtype"
  blpMipMapOffset <- replicateM 16 dword <?> "mipmaps offsets"
  blpMipMapSize <- replicateM 16 dword <?> "mipmaps sizes"
  let mipMapsInfo = nub . filter ((> 0) . snd) $ blpMipMapOffset `zip` blpMipMapSize
  blpExt <- case blpCompression of
    BlpCompressionJPEG -> blpJpegParser mipMapsInfo
    BlpCompressionUncompressed -> case blpPictureType of
      JPEGType -> fail "JPEG type with Uncompressed type mix"
      UncompressedWithAlpha -> blpUncompressed1Parser mipMapsInfo
      UncompressedWithoutAlpha -> blpUncompressed2Parser mipMapsInfo

  return $ BlpStruct {..}

blpVersion :: Parser ByteString
blpVersion = string "BLP1" <?> "BLP1 version tag"

dword :: Parser Word32
dword = do
  bs <- AT.take 4
  return . pack . BS.reverse $ bs
  where
  pack = BS.foldl' (\n h -> (n `shiftL` 8) .|. fromIntegral h) 0

rgba8 :: Parser PixelRGBA8
rgba8 = PixelRGBA8 <$> anyWord8 <*> anyWord8 <*> anyWord8 <*> anyWord8

compressionParser :: Parser BlpCompression
compressionParser = (<?> "compression") $ do
  i <- dword
  case i of
    0 -> return BlpCompressionJPEG
    1 -> return BlpCompressionUncompressed
    _ -> fail $ "Unknown compression " ++ show i

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
    2 -> return JPEGType
    3 -> return UncompressedWithAlpha
    4 -> return UncompressedWithAlpha
    5 -> return UncompressedWithoutAlpha
    _ -> fail $ "Unknown picture type " ++ show i

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
  blpU1Palette <- V.replicateM 256 rgba8
  blpU1MipMaps <- forM mps $ \(offset, size) -> do
    skipToOffset offset
    let halfSize = fromIntegral size `div` 2
    indexList <- AT.take halfSize <?> "index list"
    alphaList <- AT.take halfSize <?> "alpha list"
    return (indexList, alphaList)
  return $ BlpUncompressed1 {..}

blpUncompressed2Parser :: [(Word32, Word32)] -> Parser BlpExt
blpUncompressed2Parser mps = do
  blpU2Palette <- V.replicateM 256 rgba8
  blpU2MipMaps <- forM mps $ \(offset, size) -> do
    skipToOffset offset
    AT.take (fromIntegral size) <?> "index list"
  return $ BlpUncompressed2 {..}

parseBlp :: ByteString -> Either String BlpStruct
parseBlp = parseOnly blpParser
