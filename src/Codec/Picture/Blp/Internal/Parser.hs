{-|
Parser for BLP1 and BLP2 files. It reads the header with attoparsec and
takes the content (JPEG header, palette, mip levels) straight from the slots
the header points at; turning the content into images is done in
"Codec.Picture.Blp.Internal.Decoder".

This is an internal module: its API may change between minor versions.
-}
module Codec.Picture.Blp.Internal.Parser(
    parseBlp
  , parseBlp2
  , parseBlpFile
  , readPalette
  , blp1Tag
  , dword
  , compressionParser
  , maxSide
  , mipChainLength
  , mipLevels
  , jpegFrameSize
  ) where

import Codec.Picture
import Data.Attoparsec.ByteString (Parser, parseOnly, string, count, (<?>))
import qualified Data.Attoparsec.ByteString as AT
import Data.Bits
import Data.ByteString (ByteString)
import Data.Word

import qualified Data.Vector as V
import qualified Data.ByteString as BS

import Codec.Picture.Blp.Internal.Data
import Codec.Picture.Blp.Internal.Dxt (DxtFormat(..))
import Codec.Picture.Blp.Types (BlpDecodeError(..))

-- | Size of the fixed BLP1 header in bytes.
headerSize :: Int
headerSize = 156

-- | Largest width or height the format allows.
maxSide :: Int
maxSide = 65535

-- | Parse a BLP1 file into its header and raw content.
--
-- Width and height must be between 1 and 'maxSide'. Mip levels are taken
-- slot by slot, see 'mipLevels'.
parseBlp :: ByteString -> Either BlpDecodeError BlpStruct
parseBlp input = do
  checkMagic "BLP1" input
  (mkStruct, content, width, height, slots) <- truncated $ parseOnly headerParser input
  checkSize width height
  let levelsFrom start = mipLevels (BS.length input) start (mipChainLength (fromIntegral width) (fromIntegral height)) slots
  fmap mkStruct $ case content of
    0 -> do
      jpegHeaderSize <- truncated $ parseOnly (AT.take headerSize *> dword) input
      let headerEnd = headerSize + 4 + fromIntegral jpegHeaderSize
      if headerEnd > BS.length input
        then Left BlpTruncatedHeader
        else Right BlpJpeg {
            blpJpegHeader = slice input (headerSize + 4, fromIntegral jpegHeaderSize)
          , blpJpegData = slice input <$> levelsFrom headerEnd
          }
    1 -> do
      let levels = levelsFrom headerSize
      Right BlpPaletted {
          blpPalette = readPalette input headerSize levels
        , blpPalettedMipMaps = slice input <$> levels
        }
    c -> Left . BlpUnsupported $ "BLP1 content type " ++ show c

-- | The file must start with the given magic. A non-empty file shorter than
-- that is cut short if it is the start of the magic.
checkMagic :: ByteString -> ByteString -> Either BlpDecodeError ()
checkMagic magic input
  | BS.take 4 input == magic = Right ()
  | not (BS.null input) && BS.length input < 4 && input `BS.isPrefixOf` magic = Left BlpTruncatedHeader
  | otherwise = Left BlpNotBlp

-- | Width and height must be between 1 and 'maxSide'.
checkSize :: Word32 -> Word32 -> Either BlpDecodeError ()
checkSize width height
  | w < 1 || h < 1 || w > maxSide || h > maxSide = Left $ BlpInvalidSize w h
  | otherwise = Right ()
  where
    w = fromIntegral width
    h = fromIntegral height

-- | Once the magic is checked, the header parsers fail only when the input
-- ends.
truncated :: Either String a -> Either BlpDecodeError a
truncated = either (const (Left BlpTruncatedHeader)) Right

-- | The 256 entry palette that starts at the given offset. Some tools write
-- only the palette entries they use, so the palette ends where mip 0
-- starts. Missing entries are padded with transparent black, so any index
-- stays valid.
readPalette :: ByteString -> Int -> [(Int, Int)] -> V.Vector PixelRGBA8
readPalette input start levels = V.generate 256 $ \i ->
  if i < entries then entry i else PixelRGBA8 0 0 0 0
  where
    paletteEnd = case levels of
      (offset, _) : _ -> min offset (start + 1024)
      [] -> start + 1024
    paletteBytes = slice input (start, paletteEnd - start)
    entries = BS.length paletteBytes `div` 4
    entry i = let b k = BS.index paletteBytes (4 * i + k)
              in PixelRGBA8 (b 0) (b 1) (b 2) (b 3)

-- | The bytes at an offset and size, cut short at the end of the input.
slice :: ByteString -> (Int, Int) -> ByteString
slice input (offset, size) = BS.take size (BS.drop offset input)

-- | Parse a BLP1 or BLP2 file, told apart by their magic. Anything that is
-- not BLP2 is parsed, and rejected, as BLP1.
parseBlpFile :: ByteString -> Either BlpDecodeError BlpFile
parseBlpFile input
  | BS.take 4 input == "BLP2" = BlpFile2 <$> parseBlp2 input
  | otherwise = BlpFile1 <$> parseBlp input

-- | Size of the fixed BLP2 header, before the palette: tag, type, four
-- bytes, width, height, 16 offsets and 16 sizes.
blp2HeaderSize :: Int
blp2HeaderSize = 148

-- | Parse a BLP2 file into its header and raw content.
--
-- Only type 1 files are read; type 0 holds JPEG content, which no known
-- file uses. Width and height must be between 1 and 'maxSide', and mip
-- levels are taken slot by slot, as in BLP1 (see 'mipLevels'). Levels may
-- start right after the header: the palette that follows it is only read
-- for paletted content, and cut short where mip 0 starts.
parseBlp2 :: ByteString -> Either BlpDecodeError Blp2Struct
parseBlp2 input = do
  checkMagic "BLP2" input
  (blpType, encoding, blp2AlphaDepth, blp2AlphaType, blp2HasMips, blp2Width, blp2Height, slots) <-
    truncated $ parseOnly blp2HeaderParser input
  case blpType of
    1 -> Right ()
    0 -> Left $ BlpUnsupported "BLP2 with JPEG content (type 0)"
    t -> Left . BlpUnsupported $ "BLP2 type " ++ show t
  checkSize blp2Width blp2Height
  let levels = mipLevels (BS.length input) blp2HeaderSize (mipChainLength (fromIntegral blp2Width) (fromIntegral blp2Height)) slots
  blp2Content <- case encoding of
    1 -> Right . Blp2Paletted $ readPalette input blp2HeaderSize levels
    2 -> case blp2AlphaType of
      0 -> Right . Blp2Dxt $ Dxt1 (blp2AlphaDepth /= 0)
      1 -> Right $ Blp2Dxt Dxt3
      7 -> Right $ Blp2Dxt Dxt5
      t -> Left . BlpUnsupported $ "BLP2 DXT alpha type " ++ show t
    3 -> Right Blp2Bgra
    e -> Left . BlpUnsupported $ "BLP2 encoding " ++ show e
  return Blp2Struct { blp2MipMaps = slice input <$> levels, .. }

-- | The BLP2 header: type, encoding, alpha depth, alpha type, has mips,
-- width, height and the mipmap slots.
blp2HeaderParser :: Parser (Word32, Word8, Word8, Word8, Word8, Word32, Word32, [(Word32, Word32)])
blp2HeaderParser = do
  _ <- string "BLP2" <?> "BLP2 version tag"
  blpType <- dword <?> "type"
  encoding <- AT.anyWord8 <?> "encoding"
  alphaDepth <- AT.anyWord8 <?> "alpha depth"
  alphaType <- AT.anyWord8 <?> "alpha type"
  hasMips <- AT.anyWord8 <?> "has mips"
  width <- dword <?> "width"
  height <- dword <?> "height"
  offsets <- count 16 dword <?> "mipmaps offsets"
  sizes <- count 16 dword <?> "mipmaps sizes"
  return (blpType, encoding, alphaDepth, alphaType, hasMips, width, height, offsets `zip` sizes)

-- | The fixed header: a function completing the struct with its content,
-- plus the fields needed to read the content.
headerParser :: Parser (BlpExt -> BlpStruct, Word32, Word32, Word32, [(Word32, Word32)])
headerParser = do
  _ <- blp1Tag
  content <- dword <?> "content"
  blpAlphaBits <- dword <?> "alpha bits"
  blpWidth <- dword <?> "width"
  blpHeight <- dword <?> "height"
  blpExtra <- dword <?> "extra"
  blpHasMipmaps <- dword <?> "has mipmaps"
  offsets <- count 16 dword <?> "mipmaps offsets"
  sizes <- count 16 dword <?> "mipmaps sizes"
  let mkStruct blpExt = BlpStruct {..}
  return (mkStruct, content, blpWidth, blpHeight, offsets `zip` sizes)

-- | The @BLP1@ magic. BLP0 and BLP2 files fail here.
blp1Tag :: Parser ByteString
blp1Tag = string "BLP1" <?> "BLP1 version tag"

-- | A little-endian 32-bit word.
dword :: Parser Word32
dword = do
  bs <- AT.take 4
  return . pack . BS.reverse $ bs
  where
  pack = BS.foldl' (\n h -> (n `shiftL` 8) .|. fromIntegral h) 0

-- | Header dword @0x04@ (@content@): 0 is JPEG, 1 is paletted.
compressionParser :: Parser BlpCompression
compressionParser = (<?> "compression") $ do
  i <- dword
  case i of
    0 -> return BlpCompressionJPEG
    1 -> return BlpCompressionUncompressed
    _ -> fail $ "Unknown compression " ++ show i

-- | The mip levels a file holds, as (offset, size) in the input. Slots are
-- read in order from slot 0, and the chain ends at the first slot that is
-- empty, does not fit in the input, starts before the given content offset,
-- or overlaps an earlier level, and after the given number of levels.
--
-- Unused slots should be zero, but tools leave garbage there: offsets past
-- the end of the file, or copies of an earlier slot. Ending the chain there
-- keeps element @i@ of the result level @i@.
mipLevels :: Int -- ^ Length of the input
  -> Int -- ^ Offset where the content starts
  -> Int -- ^ Most levels to take
  -> [(Word32, Word32)] -- ^ Mipmap slots: offset and size
  -> [(Int, Int)]
mipLevels inputLen start maxLevels = go [] . take maxLevels
  where
    go used ((offset, size) : rest)
      | s > 0 && o >= toInteger start && o + s <= toInteger inputLen && not (any (overlaps (o, s)) used) =
          (fromInteger o, fromInteger s) : go ((o, s) : used) rest
      where
        o = toInteger offset
        s = toInteger size
    go _ _ = []
    overlaps (o, s) (o', s') = o < o' + s' && o' < o + s

-- | Width and height from the frame header (SOFn) of a JPEG stream, read
-- without decoding the stream.
jpegFrameSize :: ByteString -> Maybe (Int, Int)
jpegFrameSize bs
  | BS.take 2 bs == BS.pack [0xFF, 0xD8] = go 2
  | otherwise = Nothing
  where
    go i = do
      0xFF <- byte i
      marker <- byte (i + 1)
      if | marker == 0xFF -> go (i + 1) -- fill byte
         | marker == 0x01 || (marker >= 0xD0 && marker <= 0xD7) -> go (i + 2) -- no length
         | marker == 0xD9 || marker == 0xDA -> Nothing -- end of image or start of scan
         | isFrameHeader marker -> do
             height <- word16 (i + 5)
             width <- word16 (i + 7)
             return (width, height)
         | otherwise -> do
             len <- word16 (i + 2)
             if len < 2 then Nothing else go (i + 2 + len)
    byte i = if i < BS.length bs then Just (BS.index bs i) else Nothing
    word16 i = do
      hi <- byte i
      lo <- byte (i + 1)
      return (fromIntegral hi * 256 + fromIntegral lo)
    -- SOF0 to SOF15, except DHT (C4), JPG (C8) and DAC (CC).
    isFrameHeader m = m >= 0xC0 && m <= 0xCF && m `notElem` [0xC4, 0xC8, 0xCC]
