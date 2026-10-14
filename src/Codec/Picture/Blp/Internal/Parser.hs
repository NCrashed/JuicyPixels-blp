{-|
Attoparsec parser for BLP1 files. It handles the header, the mipmap slot
table and the raw content; turning the content into images is done in
"Codec.Picture.Blp".

This is an internal module: its API may change between minor versions.
-}
module Codec.Picture.Blp.Internal.Parser(
    blpParser
  , blpVersion
  , dword
  , compressionParser
  , blpJpegParser
  , getPos
  , skipToOffset
  , blpPalettedParser
  , parseBlp
  ) where

import Codec.Picture
import Control.Monad
import Data.Attoparsec.ByteString as AT
import Data.Bits
import Data.ByteString (ByteString)
import Data.List (nub, sortOn)
import Data.Word

import qualified Data.Vector as V
import qualified Data.Attoparsec.Internal.Types as AT
import qualified Data.ByteString as BS

import Codec.Picture.Blp.Internal.Data

-- | Parser for a whole BLP1 file. The argument is the length of the input,
-- used to drop mipmap slots that point past its end.
blpParser :: Int -> Parser BlpStruct
blpParser inputLen = do
  _ <- blpVersion
  blpCompression <- compressionParser
  blpAlphaBits <- dword <?> "alpha bits"
  blpWidth <- dword <?> "width"
  blpHeight <- dword <?> "height"
  blpExtra <- dword <?> "extra"
  blpHasMipmaps <- dword <?> "has mipmaps"
  blpMipMapOffset <- replicateM 16 dword <?> "mipmaps offsets"
  blpMipMapSize <- replicateM 16 dword <?> "mipmaps sizes"
  -- BLP1 specifies 16 mipmap slots but most files only use a handful; the
  -- unused slots should be zero but some real-world files contain garbage
  -- offsets/sizes left behind by the writing tool. Drop any slot whose
  -- advertised range would run past the end of the file, then sort the
  -- survivors by offset so that the monotonic `skipToOffset` walks forward
  -- correctly even if the file stored mipmaps out of order.
  let rawMipMapsInfo = nub . filter ((> 0) . snd) $ blpMipMapOffset `zip` blpMipMapSize
      fitsInput (o, s) =
        fromIntegral o + fromIntegral s <= (fromIntegral inputLen :: Integer)
      -- Sorting is not enough on its own. `skipToOffset` only ever moves
      -- forward, so a slot starting inside a range that has already been
      -- consumed can never be reached: the skip stays where it is, the
      -- following `take` reads the wrong bytes, and every later mipmap is
      -- dragged forward until the walk runs off the end of the file. Slots
      -- holding garbage that points back into mipmap 0 are common enough in
      -- the wild to matter, so keep only a forward-progressing run.
      dropOverlapping :: Integer -> [(Word32, Word32)] -> [(Word32, Word32)]
      dropOverlapping _ [] = []
      dropOverlapping end (slot@(o, s) : rest)
        | fromIntegral o >= end =
            slot : dropOverlapping (fromIntegral o + fromIntegral s) rest
        | otherwise = dropOverlapping end rest
      mipMapsInfo =
        dropOverlapping 0 . sortOn fst . filter fitsInput $ rawMipMapsInfo
  blpExt <- case blpCompression of
    BlpCompressionJPEG -> blpJpegParser mipMapsInfo
    BlpCompressionUncompressed -> blpPalettedParser mipMapsInfo

  return $ BlpStruct {..}

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

rgba8 :: Parser PixelRGBA8
rgba8 = PixelRGBA8 <$> anyWord8 <*> anyWord8 <*> anyWord8 <*> anyWord8

-- | Header dword @0x04@ (@content@): 0 is JPEG, 1 is paletted.
compressionParser :: Parser BlpCompression
compressionParser = (<?> "compression") $ do
  i <- dword
  case i of
    0 -> return BlpCompressionJPEG
    1 -> return BlpCompressionUncompressed
    _ -> fail $ "Unknown compression " ++ show i

-- | JPEG content: the shared header, then each mip level at its slot.
blpJpegParser :: [(Word32, Word32)] -> Parser BlpExt
blpJpegParser mps = (<?> "blp jpeg") $ do
  headerSize <- dword <?> "jpeg header size"
  blpJpegHeader <- AT.take (fromIntegral headerSize) <?> "jpeg header"
  blpJpegData <- forM mps $ \(offset, size) -> do
    skipToOffset offset
    AT.take $ fromIntegral size
  return $ BlpJpeg {..}

-- | Current offset into the input.
getPos :: Parser Int
getPos = AT.Parser $ \t pos more _ succ' -> succ' t pos more (AT.fromPos pos)

-- | Skip forward to an offset. Offsets behind the current position are
-- ignored, so this never moves backwards.
skipToOffset :: Word32 -> Parser ()
skipToOffset i = do
  pos <- getPos
  let diff = fromIntegral i - pos
  if diff <= 0 then return ()
    else void $ AT.take diff

-- | Read up to 256 palette entries, bounded by the offset of the first
-- mipmap. Some BLP1 files in the wild (e.g. produced by community map
-- tools) only emit the palette entries that are actually used, so the
-- palette region between the main header and the first mipmap can be
-- shorter than the canonical 1024 bytes. We pad the resulting vector to
-- 256 entries with transparent black so the rest of the codebase, which
-- indexes the palette by an arbitrary `Word8`, stays sound.
paletteParser :: [(Word32, Word32)] -> Parser (V.Vector PixelRGBA8)
paletteParser mps = do
  pos <- getPos
  let firstOffset = case mps of
        ((o, _):_) -> fromIntegral o
        []         -> pos + 1024  -- no mipmaps: keep historical behaviour
      available   = max 0 (firstOffset - pos)
      nEntries    = min 256 (available `div` 4)
  entries <- V.replicateM nEntries rgba8
  return $ entries V.++ V.replicate (256 - nEntries) (PixelRGBA8 0 0 0 0)

-- | Paletted content: the palette, then the raw data of each mip level.
-- Splitting a level into index and alpha lists needs its size, which is
-- left to the decoder.
blpPalettedParser :: [(Word32, Word32)] -> Parser BlpExt
blpPalettedParser mps = do
  blpPalette <- paletteParser mps
  blpPalettedMipMaps <- forM mps $ \(offset, size) -> do
    skipToOffset offset
    AT.take (fromIntegral size) <?> "mipmap data"
  return $ BlpPaletted {..}

-- | Parse a BLP1 file into its header and raw content.
parseBlp :: ByteString -> Either String BlpStruct
parseBlp bs = parseOnly (blpParser (BS.length bs)) bs
