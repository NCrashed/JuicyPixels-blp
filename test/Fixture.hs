-- | Byte-level BLP1 and BLP2 builders for tests.
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
  , extraOffset
  , Header2(..)
  , defHeader2
  , blp2HeaderSize
  , blp2
  , blankPalette
  , colourBlock
  , dxt3Alpha
  , dxt5Alpha
  ) where

import Data.Bits
import Data.ByteString (ByteString)
import Data.Word

import qualified Data.ByteString as BS

data Header = Header {
  hCompression :: Word32
, hAlphaBits   :: Word32
, hWidth       :: Word32
, hHeight      :: Word32
, hExtra       :: Word32
, hHasMipmaps  :: Word32
}

-- | Uncompressed, with-alpha 4x4 header; override fields as needed.
defHeader :: Header
defHeader = Header {
    hCompression = 1
  , hAlphaBits   = 8
  , hWidth       = 4
  , hHeight      = 4
  , hExtra       = 4
  , hHasMipmaps  = 5
  }

-- | Size of the fixed BLP1 header: tag, six dwords, 16 offsets, 16 sizes.
headerSize :: Int
headerSize = 156

-- | Assemble a file from a header, the mipmap slots (padded to 16 with zeros)
-- and everything that follows the header.
blpFile :: Header -> [(Word32, Word32)] -> ByteString -> ByteString
blpFile Header{..} slots body = BS.concat
  [ "BLP1"
  , dwords [hCompression, hAlphaBits, hWidth, hHeight, hExtra, hHasMipmaps]
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
extraOffset :: Int
extraOffset = 20

offsetSlot, sizeSlot :: Int -> Int
offsetSlot i = 28 + 4 * i
sizeSlot i = 92 + 4 * i

-- | BLP2 header fields.
data Header2 = Header2 {
  h2Type       :: Word32
, h2Encoding   :: Word8
, h2AlphaDepth :: Word8
, h2AlphaType  :: Word8
, h2HasMips    :: Word8
, h2Width      :: Word32
, h2Height     :: Word32
}

-- | Opaque DXT1 4x4 header; override fields as needed.
defHeader2 :: Header2
defHeader2 = Header2 {
    h2Type       = 1
  , h2Encoding   = 2
  , h2AlphaDepth = 0
  , h2AlphaType  = 0
  , h2HasMips    = 1
  , h2Width      = 4
  , h2Height     = 4
  }

-- | Size of the fixed BLP2 header: tag, type, four bytes, width, height,
-- 16 offsets, 16 sizes.
blp2HeaderSize :: Int
blp2HeaderSize = 148

-- | BLP2 file: header, palette (as given; real files always have 256
-- entries), then each mipmap back to back with slots pointing at them.
blp2 :: Header2 -> [PaletteEntry] -> [ByteString] -> ByteString
blp2 Header2{..} palette mips = BS.concat
  [ "BLP2"
  , dword h2Type
  , BS.pack [h2Encoding, h2AlphaDepth, h2AlphaType, h2HasMips]
  , dwords [h2Width, h2Height]
  , dwords (map fst slots), dwords (map snd slots)
  , pal
  , BS.concat mips
  ]
  where
    pal = paletteBytes palette
    sizes = map (fromIntegral . BS.length) mips
    offsets = scanl (+) (fromIntegral $ blp2HeaderSize + BS.length pal) sizes
    slots = take 16 $ zip offsets sizes ++ repeat (0, 0)

-- | 256 transparent black entries, as DXT and BGRA files carry.
blankPalette :: [PaletteEntry]
blankPalette = replicate 256 (0, 0, 0, 0)

-- | A DXT colour block: two RGB565 endpoints and the indices of the first
-- pixels, the rest zero.
colourBlock :: Int -> Int -> [Int] -> ByteString
colourBlock c0 c1 ixs = BS.pack $ le 2 c0 ++ le 2 c1 ++ le 4 (packBits 2 ixs)

-- | DXT3 alpha block: a 4-bit value per pixel.
dxt3Alpha :: [Int] -> ByteString
dxt3Alpha = BS.pack . le 8 . packBits 4

-- | DXT5 alpha block: two endpoints and a 3-bit index per pixel.
dxt5Alpha :: Int -> Int -> [Int] -> ByteString
dxt5Alpha a0 a1 ixs = BS.pack $ map fromIntegral [a0, a1] ++ le 6 (packBits 3 ixs)

packBits :: Int -> [Int] -> Integer
packBits bits = sum . zipWith (\i v -> toInteger v `shiftL` (bits * i)) [0 ..]

le :: Integral a => Int -> a -> [Word8]
le n v = [fromIntegral (toInteger v `shiftR` (8 * k)) | k <- [0 .. n - 1]]
