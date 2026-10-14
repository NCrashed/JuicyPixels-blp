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
  blpFlags <- flagsParser
  blpWidth <- dword <?> "width"
  blpHeight <- dword <?> "height"
  blpPictureType <- pictureTypeParser
  blpPictureSubType <- dword <?> "picture subtype"
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
  -- Some writing tools set `pictureType` to `UncompressedWithAlpha` on
  -- files whose mipmap 0 is actually only large enough for a single
  -- index plane (w*h bytes) rather than indices + alpha (2*w*h bytes).
  -- Trust the physical size over the picture-type byte and decode those
  -- files via the without-alpha path instead of crashing on a half-read
  -- index list. Anything short of 2*w*h cannot hold both planes, so the
  -- check is a range rather than an exact match.
  let pixels0 :: Integer
      pixels0 = fromIntegral blpWidth * fromIntegral blpHeight
      mipZeroSize = case mipMapsInfo of
        ((_, s):_) -> fromIntegral s :: Integer
        []         -> 0
      mislabeledAsAlpha =
        blpPictureType == UncompressedWithAlpha &&
        pixels0 > 0 &&
        mipZeroSize >= pixels0 &&
        mipZeroSize < 2 * pixels0
      effectivePictureType =
        if mislabeledAsAlpha then UncompressedWithoutAlpha else blpPictureType
  blpExt <- case blpCompression of
    BlpCompressionJPEG -> blpJpegParser mipMapsInfo
    BlpCompressionUncompressed -> case effectivePictureType of
      JPEGType -> fail "JPEG type with Uncompressed type mix"
      UncompressedWithAlpha -> blpUncompressed1Parser mipMapsInfo
      UncompressedWithoutAlpha -> blpUncompressed2Parser mipMapsInfo

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

-- | Header dword @0x08@ (@alphaBits@). Only the value 8 is recognised.
flagsParser :: Parser [BlpFlag]
flagsParser = (<?> "flags") $ do
  i <- dword
  return $ if i `testBit` 3
    then [BlpFlagAlphaChannel]
    else []

-- | Header dword @0x14@ (@extra@), read as a picture type. Unknown values
-- fail the parse.
pictureTypeParser :: Parser BlpPictureType
pictureTypeParser = (<?> "picture type") $ do
  i <- dword
  case i of
    0 -> return UncompressedWithAlpha
    2 -> return JPEGType
    3 -> return UncompressedWithAlpha
    4 -> return UncompressedWithAlpha
    5 -> return UncompressedWithoutAlpha
    -- BLP1 files seen in the wild (notably from World of Warcraft assets,
    -- as well as some BLPs found inside Warcraft III maps) use picture type
    -- 6 for JPEG-compressed textures with an alpha channel. The actual data
    -- layout is identical to type 2, and the dispatch in `blpParser` already
    -- decides JPEG vs uncompressed based on the `compression` field, so we
    -- can safely treat 6 as another spelling of `JPEGType`.
    6 -> return JPEGType
    _ -> fail $ "Unknown picture type " ++ show i

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

-- | Paletted content with 8-bit alpha: palette, then index and alpha list
-- of each mip level, each half of the slot.
blpUncompressed1Parser :: [(Word32, Word32)] -> Parser BlpExt
blpUncompressed1Parser mps = do
  blpU1Palette <- paletteParser mps
  blpU1MipMaps <- forM mps $ \(offset, size) -> do
    skipToOffset offset
    let halfSize = fromIntegral size `div` 2
    indexList <- AT.take halfSize <?> "index list"
    alphaList <- AT.take halfSize <?> "alpha list"
    return (indexList, alphaList)
  return $ BlpUncompressed1 {..}

-- | Paletted content without alpha: palette, then the index list of each
-- mip level.
blpUncompressed2Parser :: [(Word32, Word32)] -> Parser BlpExt
blpUncompressed2Parser mps = do
  blpU2Palette <- paletteParser mps
  blpU2MipMaps <- forM mps $ \(offset, size) -> do
    skipToOffset offset
    AT.take (fromIntegral size) <?> "index list"
  return $ BlpUncompressed2 {..}

-- | Parse a BLP1 file into its header and raw content.
parseBlp :: ByteString -> Either String BlpStruct
parseBlp bs = parseOnly (blpParser (BS.length bs)) bs
