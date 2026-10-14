-- | Byte-level BLP1 builder for tests.
--
-- Uncompressed files are assembled here field by field rather than with the
-- library's encoder, so decoder tests do not depend on the encoder being
-- right. JPEG files are taken from the encoder and patched with 'setDword'.
module Fixture(
    Header(..)
  , defHeader
  , headerSize
  , blpFile
  , uncompressed
  , PaletteEntry
  , paletteBytes
  , getDword
  , setDword
  , offsetSlot
  , sizeSlot
  , pictureTypeOffset
  ) where

import Data.Bits
import Data.ByteString (ByteString)
import Data.Word

import qualified Data.ByteString as BS

data Header = Header {
  hCompression :: Word32
, hFlags       :: Word32
, hWidth       :: Word32
, hHeight      :: Word32
, hPictureType :: Word32
, hSubType     :: Word32
}

-- | Uncompressed, with-alpha 4x4 header; override fields as needed.
defHeader :: Header
defHeader = Header {
    hCompression = 1
  , hFlags       = 8
  , hWidth       = 4
  , hHeight      = 4
  , hPictureType = 4
  , hSubType     = 5
  }

-- | Size of the fixed BLP1 header: tag, six dwords, 16 offsets, 16 sizes.
headerSize :: Int
headerSize = 156

-- | Assemble a file from a header, the mipmap slots (padded to 16 with zeros)
-- and everything that follows the header.
blpFile :: Header -> [(Word32, Word32)] -> ByteString -> ByteString
blpFile Header{..} slots body = BS.concat
  [ "BLP1"
  , dwords [hCompression, hFlags, hWidth, hHeight, hPictureType, hSubType]
  , dwords (map fst slots') , dwords (map snd slots')
  , body
  ]
  where
    slots' = take 16 $ slots ++ repeat (0, 0)

-- | Palette entry as it is stored in the file: blue, green, red, alpha.
type PaletteEntry = (Word8, Word8, Word8, Word8)

paletteBytes :: [PaletteEntry] -> ByteString
paletteBytes es = BS.pack $ concat [[b, g, r, a] | (b, g, r, a) <- es]

-- | Uncompressed file: header, palette, then each mipmap back to back with
-- slots pointing at them.
uncompressed :: Header -> [PaletteEntry] -> [ByteString] -> ByteString
uncompressed h palette mips = blpFile h slots (pal <> BS.concat mips)
  where
    pal = paletteBytes palette
    sizes = map (fromIntegral . BS.length) mips
    offsets = scanl (+) (fromIntegral $ headerSize + BS.length pal) sizes
    slots = zip offsets sizes

dwords :: [Word32] -> ByteString
dwords = BS.concat . map dword

dword :: Word32 -> ByteString
dword v = BS.pack [fromIntegral (v `shiftR` (8 * i)) | i <- [0 .. 3]]

getDword :: Int -> ByteString -> Word32
getDword o bs = foldr (\i acc -> acc `shiftL` 8 .|. fromIntegral (BS.index bs (o + i))) 0 [0 .. 3]

setDword :: Int -> Word32 -> ByteString -> ByteString
setDword o v bs = BS.take o bs <> dword v <> BS.drop (o + 4) bs

-- | File offsets of the header fields that tests patch.
pictureTypeOffset :: Int
pictureTypeOffset = 20

offsetSlot, sizeSlot :: Int -> Int
offsetSlot i = 28 + 4 * i
sizeSlot i = 92 + 4 * i
