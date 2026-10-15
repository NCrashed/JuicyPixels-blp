{-|
Parser for BLP1 files. It reads the header with attoparsec and takes the
content (JPEG header, palette, mip levels) straight from the slots the header
points at; turning the content into images is done in "Codec.Picture.Blp".

This is an internal module: its API may change between minor versions.
-}
module Codec.Picture.Blp.Internal.Parser(
    parseBlp
  , blpVersion
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
parseBlp :: ByteString -> Either String BlpStruct
parseBlp input = do
  (mkStruct, compression, width, height, slots) <- parseOnly headerParser input
  let w = fromIntegral width
      h = fromIntegral height
  if w < 1 || h < 1 || w > maxSide || h > maxSide
    then Left $ "Invalid image size " ++ show width ++ "x" ++ show height
    else Right ()
  let levelsFrom start = mipLevels (BS.length input) start (mipChainLength w h) slots
      slice (offset, size) = BS.take size (BS.drop offset input)
  fmap mkStruct $ case compression of
    BlpCompressionJPEG -> do
      jpegHeaderSize <- parseOnly (AT.take headerSize *> dword <?> "jpeg header size") input
      let headerEnd = headerSize + 4 + fromIntegral jpegHeaderSize
      if headerEnd > BS.length input
        then Left "jpeg header: not enough input"
        else Right BlpJpeg {
            blpJpegHeader = slice (headerSize + 4, fromIntegral jpegHeaderSize)
          , blpJpegData = slice <$> levelsFrom headerEnd
          }
    BlpCompressionUncompressed -> do
      let levels = levelsFrom headerSize
          -- Some tools write only the palette entries they use, so the
          -- palette ends where mip 0 starts. Missing entries are padded
          -- with transparent black, so any index stays valid.
          paletteEnd = case levels of
            (offset, _) : _ -> min offset (headerSize + 1024)
            [] -> headerSize + 1024
          paletteBytes = slice (headerSize, paletteEnd - headerSize)
          entries = BS.length paletteBytes `div` 4
          entry i = let b k = BS.index paletteBytes (4 * i + k)
                    in PixelRGBA8 (b 0) (b 1) (b 2) (b 3)
      Right BlpPaletted {
          blpPalette = V.generate 256 $ \i ->
            if i < entries then entry i else PixelRGBA8 0 0 0 0
        , blpPalettedMipMaps = slice <$> levels
        }

-- | The fixed header: a function completing the struct with its content,
-- plus the fields needed to read the content.
headerParser :: Parser (BlpExt -> BlpStruct, BlpCompression, Word32, Word32, [(Word32, Word32)])
headerParser = do
  _ <- blpVersion
  compression <- compressionParser
  blpAlphaBits <- dword <?> "alpha bits"
  blpWidth <- dword <?> "width"
  blpHeight <- dword <?> "height"
  blpExtra <- dword <?> "extra"
  blpHasMipmaps <- dword <?> "has mipmaps"
  offsets <- count 16 dword <?> "mipmaps offsets"
  sizes <- count 16 dword <?> "mipmaps sizes"
  let mkStruct blpExt = BlpStruct {..}
  return (mkStruct, compression, blpWidth, blpHeight, offsets `zip` sizes)

-- | The @BLP1@ magic. BLP0 and BLP2 files fail here.
blpVersion :: Parser ByteString
blpVersion = string "BLP1" <?> "BLP1 version tag"

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

-- | Number of levels in a full mip chain: halving the larger side down to 1,
-- at most 16.
mipChainLength :: Int -> Int -> Int
mipChainLength w h = min 16 . length . takeWhile (>= 1) $ iterate (`div` 2) (max w h)

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
