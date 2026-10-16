{-|
Decoding of S3TC (DXT1, DXT3 and DXT5) block compressed images, the
compressed content of BLP2 textures.

The image is cut into 4x4 blocks, stored row by row. A DXT1 block is 8 bytes
of colour; DXT3 and DXT5 blocks put 8 bytes of alpha before the colour. A
colour block holds two RGB565 endpoints and a 2-bit index per pixel, pixel
@(x, y)@ of the block at bits @2 * (4 * y + x)@:

* endpoints expand to 8 bits by bit replication, so 31 becomes 255;
* if the first endpoint is larger, or the block is part of DXT3 or DXT5,
  indices 2 and 3 are @(2 * c0 + c1) / 3@ and @(c0 + 2 * c1) / 3@;
* otherwise index 2 is @(c0 + c1) / 2@ and index 3 is black, transparent in
  DXT1 with alpha.

DXT3 alpha is a 4-bit value per pixel, times 17. DXT5 alpha has two 8-bit
endpoints and a 3-bit index per pixel: six interpolated values between them
if the first is larger, otherwise four plus 0 and 255.

Divisions round down, as in Pillow. Graphics hardware is not consistent
about this, so other decoders can differ by one.

This is an internal module: its API may change between minor versions.
-}
module Codec.Picture.Blp.Internal.Dxt(
    DxtFormat(..)
  , dxtLevelBytes
  , decodeDxt
  ) where

import Codec.Picture
import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Bits
import Data.ByteString (ByteString)
import Data.Word

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as MVS

-- | Block compression formats.
data DxtFormat =
    -- | 8-byte blocks of colour only. With 'True', index 3 of a three-colour
    -- block is transparent; with 'False', it is opaque black.
    Dxt1 !Bool
    -- | 16-byte blocks: explicit 4-bit alpha, then a DXT1 colour block.
  | Dxt3
    -- | 16-byte blocks: interpolated alpha, then a DXT1 colour block.
  | Dxt5
  deriving (Eq, Show)

-- | Bytes of one block.
blockBytes :: DxtFormat -> Int
blockBytes (Dxt1 _) = 8
blockBytes _ = 16

-- | Bytes a @w@ x @h@ image takes: every side is rounded up to whole blocks.
dxtLevelBytes :: DxtFormat -> Int -> Int -> Int
dxtLevelBytes fmt w h = ((w + 3) `div` 4) * ((h + 3) `div` 4) * blockBytes fmt

-- | Decode a @w@ x @h@ image. Bytes after 'dxtLevelBytes' are ignored; fewer
-- bytes, or a side below 1, give 'Left'.
decodeDxt :: DxtFormat -> Int -> Int -> ByteString -> Either String (Image PixelRGBA8)
decodeDxt fmt w h bs
  | w < 1 || h < 1 = Left $ "Invalid DXT image size " ++ show w ++ "x" ++ show h
  | BS.length bs < needed = Left $ "DXT data is " ++ show (BS.length bs)
      ++ " bytes, a " ++ show w ++ "x" ++ show h ++ " image needs " ++ show needed
  | otherwise = Right . Image w h $ VS.create $ do
      -- Every pixel belongs to a block, so every byte is written.
      dst <- MVS.unsafeNew (w * h * 4)
      -- The four colours of the current block as RGBA, then its eight DXT5
      -- alpha values. Pixels are copied from here by index: choosing with
      -- branches is several times slower, as indices are unpredictable.
      table <- MVS.unsafeNew 24
      let rows by = if by >= blocksHigh then return () else cols by 0 >> rows (by + 1)
          cols by bx = if bx >= blocksWide then return () else do
            decodeBlock fmt bs ((by * blocksWide + bx) * blockBytes fmt) w h (bx * 4) (by * 4) table dst
            cols by (bx + 1)
      rows 0
      return dst
  where
    needed = dxtLevelBytes fmt w h
    blocksWide = (w + 3) `div` 4
    blocksHigh = (h + 3) `div` 4

-- | Decode the block at byte offset @o@ into the pixels from @(x0, y0)@ that
-- fall inside the image, using @table@ as scratch space.
decodeBlock :: DxtFormat -> ByteString -> Int -> Int -> Int -> Int -> Int
  -> MVS.MVector s Word8 -> MVS.MVector s Word8 -> ST s ()
decodeBlock fmt bs o w h x0 y0 table dst = do
  colour 0 r0 g0 b0 255
  colour 1 r1 g1 b1 255
  colour 2 (mix r0 r1) (mix g0 g1) (mix b0 b1) 255
  colour 3 (last3 r0 r1) (last3 g0 g1) (last3 b0 b1) alpha3
  case fmt of
    Dxt5 -> mapM_ (\j -> MVS.unsafeWrite table (16 + j) (fromIntegral (dxt5Alpha j))) [0 .. 7]
    _ -> return ()
  pixels 0
  where
    byte i = fromIntegral (BSU.unsafeIndex bs i) :: Int
    -- Little-endian word of n bytes from offset i.
    word :: Int -> Int -> Word64
    word i n = loop (n - 1) 0
      where
        loop j !acc
          | j < 0 = acc
          | otherwise = loop (j - 1) (acc `shiftL` 8 .|. fromIntegral (byte (i + j)))

    !colourAt = case fmt of
      Dxt1 _ -> o
      _ -> o + 8
    !c0 = byte colourAt .|. byte (colourAt + 1) `shiftL` 8
    !c1 = byte (colourAt + 2) .|. byte (colourAt + 3) `shiftL` 8
    !indices = word (colourAt + 4) 4
    !fourColours = case fmt of
      Dxt1 _ -> c0 > c1
      _ -> True
    !r0 = expand5 (c0 `shiftR` 11)
    !g0 = expand6 (c0 `shiftR` 5)
    !b0 = expand5 c0
    !r1 = expand5 (c1 `shiftR` 11)
    !g1 = expand6 (c1 `shiftR` 5)
    !b1 = expand5 c1
    mix a b = if fourColours then (2 * a + b) `quot` 3 else (a + b) `quot` 2
    last3 a b = if fourColours then (a + 2 * b) `quot` 3 else 0
    !alpha3 = case fmt of
      Dxt1 True | not fourColours -> 0
      _ -> 255 :: Int

    colour k r g b a = do
      MVS.unsafeWrite table (4 * k) (fromIntegral r)
      MVS.unsafeWrite table (4 * k + 1) (fromIntegral g)
      MVS.unsafeWrite table (4 * k + 2) (fromIntegral b)
      MVS.unsafeWrite table (4 * k + 3) (fromIntegral (a `asTypeOf` r))

    -- DXT3: 4-bit alpha per pixel. DXT5: two endpoints and 3-bit indices.
    !alphaWord = case fmt of
      Dxt1 _ -> 0
      Dxt3 -> word o 8
      Dxt5 -> word (o + 2) 6
    !a0 = byte o
    !a1 = byte (o + 1)
    dxt5Alpha :: Int -> Int
    dxt5Alpha j = case j of
      0 -> a0
      1 -> a1
      _ | a0 > a1 -> ((8 - j) * a0 + (j - 1) * a1) `quot` 7
        | j < 6 -> ((6 - j) * a0 + (j - 1) * a1) `quot` 5
        | j == 6 -> 0
        | otherwise -> 255

    -- Copying a pixel as one 32-bit word keeps its bytes in order on any
    -- platform.
    table32 = asWord32 table
    dst32 = asWord32 dst

    pixels !i
      | i >= 16 = return ()
      | otherwise = do
          let x = x0 + i .&. 3
              y = y0 + i `shiftR` 2
              k = fromIntegral (indices `unsafeShiftR` (2 * i)) .&. 3
              p = y * w + x
          when (x < w && y < h) $ do
            MVS.unsafeRead table32 k >>= MVS.unsafeWrite dst32 p
            case fmt of
              Dxt1 _ -> return ()
              Dxt3 -> MVS.unsafeWrite dst (4 * p + 3) (17 * (fromIntegral (alphaWord `unsafeShiftR` (4 * i)) .&. 15))
              Dxt5 -> MVS.unsafeRead table (16 + fromIntegral (alphaWord `unsafeShiftR` (3 * i)) .&. 7)
                >>= MVS.unsafeWrite dst (4 * p + 3)
          pixels (i + 1)

-- | Widen a 5 or 6-bit channel, taken from the low bits, to 8 bits by
-- repeating its high bits.
expand5, expand6 :: Int -> Int
expand5 v = let c = v .&. 31 in c `shiftL` 3 .|. c `shiftR` 2
expand6 v = let c = v .&. 63 in c `shiftL` 2 .|. c `shiftR` 4

asWord32 :: MVS.MVector s Word8 -> MVS.MVector s Word32
asWord32 = MVS.unsafeCast
