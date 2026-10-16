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
  , encodeDxt
  ) where

import Codec.Picture
import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import Data.Bits
import qualified Data.List as L
import Data.ByteString (ByteString)
import Data.Word

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
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
    dxt5Alpha = dxt5AlphaValue a0 a1

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

-- | Alpha value of 3-bit index @j@ in a DXT5 block with endpoints @a0@ and
-- @a1@: six interpolated values between them if @a0 > a1@, otherwise four
-- plus 0 and 255.
dxt5AlphaValue :: Int -> Int -> Int -> Int
dxt5AlphaValue a0 a1 j = case j of
  0 -> a0
  1 -> a1
  _ | a0 > a1 -> ((8 - j) * a0 + (j - 1) * a1) `quot` 7
    | j < 6 -> ((6 - j) * a0 + (j - 1) * a1) `quot` 5
    | j == 6 -> 0
    | otherwise -> 255

------------------------------------------------------------------------------
-- Encoding

-- | Encode an image as DXT blocks, 'dxtLevelBytes' long.
--
-- Colours are fitted per block along the principal axis of the block's
-- pixels (range fit), and the endpoints are then refined by least squares
-- for the chosen indices while that lowers the error. Errors are measured
-- on what 'decodeDxt' gives back. DXT1 with 'True' writes blocks with
-- pixels whose alpha is below 128 in three-colour mode, those pixels
-- transparent; with 'False' alpha is ignored. DXT3 alpha is rounded to 4
-- bits; DXT5 alpha takes whichever of its two modes fits the block better.
-- Pixels outside the image, in blocks on the right and bottom edges, do not
-- count.
encodeDxt :: DxtFormat -> Image PixelRGBA8 -> ByteString
encodeDxt fmt img = BI.fromForeignPtr fptr 0 len
  where
    w = imageWidth img
    h = imageHeight img
    src = imageData img
    blocksWide = (w + 3) `div` 4
    blocksHigh = (h + 3) `div` 4
    size = blockBytes fmt
    (fptr, len) = VS.unsafeToForeignPtr0 $ VS.create $ do
      dst <- MVS.new (blocksWide * blocksHigh * size)
      forM_ [0 .. blocksHigh - 1] $ \by -> forM_ [0 .. blocksWide - 1] $ \bx ->
        encodeBlock fmt (blockPixels bx by) dst ((by * blocksWide + bx) * size)
      return dst
    -- The pixels of a block inside the image, in order, with their index in
    -- the block.
    blockPixels bx by
      | 4 * bx + 3 < w && 4 * by + 3 < h = gather wholeBlock
      | otherwise = gather (VU.filter (\i -> 4 * bx + i .&. 3 < w && 4 * by + i `shiftR` 2 < h) wholeBlock)
      where
        gather pos =
          let channel c = VU.map (\i -> fromIntegral (VS.unsafeIndex src (4 * ((4 * by + i `shiftR` 2) * w + 4 * bx + i .&. 3) + c))) pos
          in Pixels pos (channel 0) (channel 1) (channel 2) (channel 3)

-- | The positions of a block's 16 pixels.
wholeBlock :: VU.Vector Int
wholeBlock = VU.enumFromN 0 16

-- | Some of a block's pixels: their index in the block and their channels.
data Pixels = Pixels !(VU.Vector Int) !(VU.Vector Int) !(VU.Vector Int) !(VU.Vector Int) !(VU.Vector Int)

-- | Write one block at the given byte offset.
encodeBlock :: DxtFormat -> Pixels -> MVS.MVector s Word8 -> Int -> ST s ()
encodeBlock fmt px dst o = case fmt of
  Dxt1 transparent -> encodeColour transparent False px dst o
  Dxt3 -> dxt3AlphaBlock px dst o >> encodeColour False True px dst (o + 8)
  Dxt5 -> dxt5AlphaBlock px dst o >> encodeColour False True px dst (o + 8)

-- | Write the low n bytes of a number, least significant first.
putLE :: MVS.MVector s Word8 -> Int -> Int -> Int -> ST s ()
putLE dst o n v = forM_ [0 .. n - 1] $ \k -> MVS.unsafeWrite dst (o + k) (fromIntegral (v `shiftR` (8 * k)))

-- | A colour block. With @transparent@, pixels with alpha below 128 make the
-- block three-colour with them at index 3. With @fourAlways@ (DXT3 and
-- DXT5), the decoder uses four colours whatever the endpoint order.
encodeColour :: Bool -> Bool -> Pixels -> MVS.MVector s Word8 -> Int -> ST s ()
encodeColour transparent fourAlways (Pixels pos rs gs bs as) dst o = do
  putLE dst o 2 c0
  putLE dst (o + 2) 2 c1
  putLE dst (o + 4) 4 (indices .|. clearBits)
  where
    isClear j = transparent && VU.unsafeIndex as j < 128
    keep = VU.filter (not . isClear) (VU.enumFromN 0 (VU.length pos))
    clearBits = VU.foldl' (\acc j -> if isClear j then acc .|. 3 `shiftL` (2 * VU.unsafeIndex pos j) else acc) 0 (VU.enumFromN 0 (VU.length pos))
    threeColour = VU.length keep < VU.length pos
    -- The pixels the colours are fitted to.
    fpos = VU.map (VU.unsafeIndex pos) keep
    pr = VU.map (VU.unsafeIndex rs) keep
    pg = VU.map (VU.unsafeIndex gs) keep
    pb = VU.map (VU.unsafeIndex bs) keep
    n = VU.length keep

    -- A candidate replaces the best only with a strictly lower error, so
    -- none is tried once the error is 0, nor endpoints already tried.
    Fit c0 c1 _ indices
      | n == 0 = Fit 0 0 0 0 -- all transparent
      | otherwise =
          let fitted = refine (3 :: Int) (assign (endpoints (rangeFit pr pg pb)))
          in if fitError fitted == 0 then fitted else better fitted (assign single)
    better a b = if fitError b < fitError a then b else a
    refine 0 best = best
    refine k best@(Fit e0 e1 err _)
      | err == 0 || ends == (e0, e1) = best
      | fitError next < err = refine (k - 1) next
      | otherwise = best
      where
        ends = endpoints (leastSquares best)
        next = assign ends

    -- Endpoints in the order the mode needs: c0 > c1 for four colours,
    -- c0 <= c1 for three.
    endpoints (a, b)
      | threeColour = (min qa qb, max qa qb)
      | otherwise = (max qa qb, min qa qb)
      where
        qa = pack565 a
        qb = pack565 b

    -- The nearest usable colour for every pixel, packed as 2-bit indices by
    -- position in the block, and the total error.
    assign (e0, e1) =
      let four = fourAlways || e0 > e1
          pal = palette four e0 e1
          -- The error of colour k, times 4, plus k: the smallest is the
          -- nearest colour, the lower index on ties. One number instead of a
          -- pair keeps the loop free of allocation.
          nearest j =
            let r = VU.unsafeIndex pr j; g = VU.unsafeIndex pg j; b = VU.unsafeIndex pb j
                cost k = 4 * (sq (r - VU.unsafeIndex pal (3 * k)) + sq (g - VU.unsafeIndex pal (3 * k + 1)) + sq (b - VU.unsafeIndex pal (3 * k + 2))) + k
                three = min (cost 0) (min (cost 1) (cost 2))
            in if four then min three (cost 3) else three
          loop j !err !bits
            | j >= n = Fit e0 e1 err bits
            | otherwise =
                let c = nearest j
                in loop (j + 1) (err + c `shiftR` 2) (bits .|. (c .&. 3) `shiftL` (2 * VU.unsafeIndex fpos j))
      in loop 0 0 0

    -- Endpoints that best reproduce the pixels with their current indices:
    -- index k stands for weight w_k of the first endpoint and 1 - w_k of the
    -- second.
    leastSquares (Fit e0 e1 _ packed) =
      let four = fourAlways || e0 > e1
          weight :: Int -> Double
          weight k = case k of
            0 -> 1
            1 -> 0
            2 -> if four then 2 / 3 else 1 / 2
            _ -> 1 / 3
          step (Sums aa bb2 ab2 ar0 ag0 ab0 br0 bg0 bb0) j =
            let a = weight ((packed `shiftR` (2 * VU.unsafeIndex fpos j)) .&. 3)
                b' = 1 - a
                fr = fromIntegral (VU.unsafeIndex pr j)
                fg = fromIntegral (VU.unsafeIndex pg j)
                fb = fromIntegral (VU.unsafeIndex pb j)
            in Sums (aa + a * a) (bb2 + b' * b') (ab2 + a * b')
                 (ar0 + a * fr) (ag0 + a * fg) (ab0 + a * fb)
                 (br0 + b' * fr) (bg0 + b' * fg) (bb0 + b' * fb)
          Sums saa sbb sab ar ag ab br bg bb = L.foldl' step (Sums 0 0 0 0 0 0 0 0 0) [0 .. n - 1]
          det = saa * sbb - sab * sab
          sap = (ar, ag, ab)
          sbp = (br, bg, bb)
      in if abs det < 1e-9
           then rangeFit pr pg pb
           else ( scale (1 / det) (sub (scale sbb sap) (scale sab sbp))
                , scale (1 / det) (sub (scale saa sbp) (scale sab sap)) )

    -- A block of nearly one colour is best served by endpoints whose
    -- intermediate colour decodes to it exactly, which the range fit cannot
    -- find: it rounds both endpoints to the nearest RGB565 colour.
    single =
      let avg v = (VU.sum v + n `div` 2) `div` n
          table5 = if threeColour then three5 else four5
          table6 = if threeColour then three6 else four6
          (r0, r1) = table5 VU.! avg pr
          (g0, g1) = table6 VU.! avg pg
          (b0, b1) = table5 VU.! avg pb
          e0 = r0 `shiftL` 11 .|. g0 `shiftL` 5 .|. b0
          e1 = r1 `shiftL` 11 .|. g1 `shiftL` 5 .|. b1
      in if threeColour then (min e0 e1, max e0 e1) else (max e0 e1, min e0 e1)

sq :: Int -> Int
sq x = x * x

-- | Endpoints of a colour block, the error of the pixels, and their 2-bit
-- indices packed by position in the block.
data Fit = Fit !Int !Int !Int !Int

fitError :: Fit -> Int
fitError (Fit _ _ err _) = err

-- | Nine running sums, for the least squares fit and the covariance.
data Sums = Sums !Double !Double !Double !Double !Double !Double !Double !Double !Double

-- | Two points spanning the pixels along their principal axis.
rangeFit :: VU.Vector Int -> VU.Vector Int -> VU.Vector Int -> (V3, V3)
rangeFit pr pg pb = (along hi, along lo)
  where
    count = VU.length pr
    pointAt j = (fromIntegral (VU.unsafeIndex pr j), fromIntegral (VU.unsafeIndex pg j), fromIntegral (VU.unsafeIndex pb j)) :: V3
    -- Sums of the channels and of their products, in one pass.
    moment (Sums r g b rr gg bb rg rb gb) j =
      let (x, y, z) = pointAt j
      in Sums (r + x) (g + y) (b + z) (rr + x * x) (gg + y * y) (bb + z * z) (rg + x * y) (rb + x * z) (gb + y * z)
    Sums sr sg sb srr sgg sbb srg srb sgb = L.foldl' moment (Sums 0 0 0 0 0 0 0 0 0) [0 .. count - 1]
    n = fromIntegral count
    m@(mr, mg, mb) = (sr / n, sg / n, sb / n)
    -- Covariance matrix, times n.
    cxx = srr - n * mr * mr; cyy = sgg - n * mg * mg; czz = sbb - n * mb * mb
    cxy = srg - n * mr * mg; cxz = srb - n * mr * mb; cyz = sgb - n * mg * mb
    cov (x, y, z) = (cxx * x + cxy * y + cxz * z, cxy * x + cyy * y + cyz * z, cxz * x + cyz * y + czz * z)
    power v = let v' = cov v; l = norm v' in if l < 1e-9 then v else scale (1 / l) v'
    -- Start from the pixel farthest from the mean: unlike a fixed vector, it
    -- is never orthogonal to the spread, which would collapse the fit.
    farthest = snd $ L.foldl' (\best@(bd, _) j -> let d = sub (pointAt j) m; l = dot d d in if l > bd then (l, d) else best) (0, (1, 1, 1)) [0 .. count - 1]
    axis = iterate power farthest !! 8
    (lo, hi) = L.foldl' (\(a, b) j -> let t = dot (sub (pointAt j) m) axis in (min a t, max b t)) (1 / 0, -1 / 0) [0 .. count - 1]
    along t = add m (scale t axis)

type V3 = (Double, Double, Double)

add, sub :: V3 -> V3 -> V3
add (a, b, c) (d, e, f) = (a + d, b + e, c + f)
sub (a, b, c) (d, e, f) = (a - d, b - e, c - f)

scale :: Double -> V3 -> V3
scale k (a, b, c) = (k * a, k * b, k * c)

dot :: V3 -> V3 -> Double
dot (a, b, c) (d, e, f) = a * d + b * e + c * f

norm :: V3 -> Double
norm v = sqrt (dot v v)

-- | The nearest RGB565 colour.
pack565 :: V3 -> Int
pack565 (r, g, b) = q r 31 `shiftL` 11 .|. q g 63 `shiftL` 5 .|. q b 31
  where q v m = max 0 . min m $ round (v * fromIntegral m / 255)

-- | The four colours of a block as 'decodeDxt' computes them, as R, G, B
-- triples.
palette :: Bool -> Int -> Int -> VU.Vector Int
palette four e0 e1 = VU.fromListN 12
  [ r0, g0, b0, r1, g1, b1
  , mix2 r0 r1, mix2 g0 g1, mix2 b0 b1
  , mix3 r0 r1, mix3 g0 g1, mix3 b0 b1 ]
  where
    (r0, g0, b0) = (expand5 (e0 `shiftR` 11), expand6 (e0 `shiftR` 5), expand5 e0)
    (r1, g1, b1) = (expand5 (e1 `shiftR` 11), expand6 (e1 `shiftR` 5), expand5 e1)
    mix2 a b = if four then (2 * a + b) `quot` 3 else (a + b) `quot` 2
    mix3 a b = if four then (a + 2 * b) `quot` 3 else 0

-- | DXT3 alpha: 4 bits per pixel, rounded.
dxt3AlphaBlock :: Pixels -> MVS.MVector s Word8 -> Int -> ST s ()
dxt3AlphaBlock (Pixels pos _ _ _ as) dst o = putLE dst o 8 bits
  where
    bits = VU.ifoldl' (\acc j a -> acc .|. min 15 ((a + 8) `div` 17) `shiftL` (4 * VU.unsafeIndex pos j)) 0 as

-- | DXT5 alpha: endpoints and 3-bit indices, in whichever mode has the
-- smaller squared error. The six-step mode spans the block's alphas; the
-- four-step mode spans those other than 0 and 255, which it has exactly.
dxt5AlphaBlock :: Pixels -> MVS.MVector s Word8 -> Int -> ST s ()
dxt5AlphaBlock (Pixels pos _ _ _ as) dst o = do
  MVS.unsafeWrite dst o (fromIntegral a0)
  MVS.unsafeWrite dst (o + 1) (fromIntegral a1)
  putLE dst (o + 2) 6 bits
  where
    inner = VU.filter (\v -> v /= 0 && v /= 255) as
    six = (VU.maximum as, VU.minimum as)
    four = if VU.null inner then (0, 0) else (VU.minimum inner, VU.maximum inner)
    -- The nearest table entry for every alpha, packed as 3-bit indices by
    -- position, and the total error. As for colours, the error times 8
    -- plus the index finds both at once.
    assign (e0, e1) =
      let table = VU.generate 8 (dxt5AlphaValue e0 e1)
          nearest v = VU.minimum (VU.imap (\j t -> 8 * sq (v - t) + j) table)
          loop j !err !packed
            | j >= VU.length as = (err, packed)
            | otherwise =
                let c = nearest (VU.unsafeIndex as j)
                in loop (j + 1) (err + c `shiftR` 3) (packed .|. (c .&. 7) `shiftL` (3 * VU.unsafeIndex pos j))
      in loop 0 (0 :: Int) (0 :: Int)
    ((a0, a1), (_, bits))
      | fst sixFit <= fst fourFit = (six, sixFit)
      | otherwise = (four, fourFit)
      where
        sixFit = assign six
        fourFit = assign four

-- | For each 8-bit value, the pair of 5 or 6-bit endpoints whose
-- intermediate colour decodes nearest to it: the first third point in
-- four-colour mode, the midpoint in three-colour mode.
four5, four6, three5, three6 :: VU.Vector (Int, Int)
four5 = singleColourTable 5 (\a b -> (2 * a + b) `quot` 3)
four6 = singleColourTable 6 (\a b -> (2 * a + b) `quot` 3)
three5 = singleColourTable 5 (\a b -> (a + b) `quot` 2)
three6 = singleColourTable 6 (\a b -> (a + b) `quot` 2)

singleColourTable :: Int -> (Int -> Int -> Int) -> VU.Vector (Int, Int)
singleColourTable bits mix = VU.generate 256 best
  where
    expand = if bits == 5 then expand5 else expand6
    values = [0 .. 2 ^ bits - 1]
    best v = snd $ minimum [ (abs (mix (expand a) (expand b) - v), (a, b)) | a <- values, b <- values ]
