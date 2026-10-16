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
import Control.Monad (forM_, when, zipWithM_)
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
    blocksWide = (w + 3) `div` 4
    blocksHigh = (h + 3) `div` 4
    size = blockBytes fmt
    (fptr, len) = VS.unsafeToForeignPtr0 $ VS.create $ do
      dst <- MVS.new (blocksWide * blocksHigh * size)
      forM_ [0 .. blocksHigh - 1] $ \by -> forM_ [0 .. blocksWide - 1] $ \bx ->
        zipWithM_ (\i b -> MVS.unsafeWrite dst ((by * blocksWide + bx) * size + i) b) [0 ..]
          (encodeBlock fmt (blockPixels bx by))
      return dst
    -- The pixels of a block inside the image, with their index in the block.
    blockPixels bx by =
      [ (4 * y + x, pixelAt img px py)
      | y <- [0 .. 3], x <- [0 .. 3]
      , let px = 4 * bx + x
      , let py = 4 * by + y
      , px < w, py < h ]

-- | The bytes of one block.
encodeBlock :: DxtFormat -> [(Int, PixelRGBA8)] -> [Word8]
encodeBlock fmt px = case fmt of
  Dxt1 transparent -> encodeColour transparent False px
  Dxt3 -> dxt3AlphaBytes px ++ encodeColour False True px
  Dxt5 -> dxt5AlphaBytes px ++ encodeColour False True px

type RGB = (Int, Int, Int)

-- | A colour block. With @transparent@, pixels with alpha below 128 make the
-- block three-colour with them at index 3. With @fourAlways@ (DXT3 and
-- DXT5), the decoder uses four colours whatever the endpoint order.
encodeColour :: Bool -> Bool -> [(Int, PixelRGBA8)] -> [Word8]
encodeColour transparent fourAlways px
  | null opaque = block 0 0 [(i, 3) | (i, _) <- px] -- all transparent
  | otherwise = block c0 c1 (zip (map fst opaque) ks ++ [(i, 3) | i <- clear])
  where
    isTransparent (PixelRGBA8 _ _ _ a) = transparent && a < 128
    opaque = [(i, rgbOf p) | (i, p) <- px, not (isTransparent p)]
    clear = [i | (i, p) <- px, isTransparent p]
    threeColour = not (null clear)
    rgbOf (PixelRGBA8 r g b _) = (fromIntegral r, fromIntegral g, fromIntegral b)
    points = map snd opaque

    -- Start from the range fit, then refine while the error drops.
    Fit c0 c1 _ ks = better (refine (3 :: Int) (assign (endpoints (rangeFit points)))) (assign single)
    better a b = if fitError b < fitError a then b else a

    -- A block of nearly one colour is best served by endpoints whose
    -- intermediate colour decodes to it exactly, which the range fit cannot
    -- find: it rounds both endpoints to the nearest RGB565 colour.
    single =
      let n = length points
          avg f = (sum (map f points) + n `div` 2) `div` n
          (r, g, b) = (avg (\(x, _, _) -> x), avg (\(_, y, _) -> y), avg (\(_, _, z) -> z))
          table5 = if threeColour then three5 else four5
          table6 = if threeColour then three6 else four6
          (r0, r1) = table5 VU.! r
          (g0, g1) = table6 VU.! g
          (b0, b1) = table5 VU.! b
          e0 = r0 `shiftL` 11 .|. g0 `shiftL` 5 .|. b0
          e1 = r1 `shiftL` 11 .|. g1 `shiftL` 5 .|. b1
      in if threeColour then (min e0 e1, max e0 e1) else (max e0 e1, min e0 e1)
    refine 0 best = best
    refine n best =
      let next = assign (endpoints (leastSquares best))
      in if fitError next < fitError best then refine (n - 1) next else best

    -- Endpoints in the order the mode needs: c0 > c1 for four colours,
    -- c0 <= c1 for three.
    endpoints (a, b)
      | threeColour = (min qa qb, max qa qb)
      | otherwise = (max qa qb, min qa qb)
      where
        qa = pack565 a
        qb = pack565 b

    -- The nearest usable colour for every pixel, and the total error.
    assign (e0, e1) =
      let four = fourAlways || e0 > e1
          pal = take (if four then 4 else 3) (zip [0 ..] (palette four e0 e1))
          nearest p = L.foldl' (\best@(bd, _) (k, c) -> let d = dist p c in if d < bd then (d, k) else best) (maxBound, 0) pal
          picks = map nearest points
      in Fit e0 e1 (sum (map fst picks)) (map snd picks)

    -- Endpoints that best reproduce the pixels with their current indices:
    -- index k stands for weight w_k of the first endpoint and 1 - w_k of the
    -- second.
    leastSquares (Fit e0 e1 _ picked) =
      let four = fourAlways || e0 > e1
          weight :: Int -> Double
          weight k = case k of
            0 -> 1
            1 -> 0
            2 -> if four then 2 / 3 else 1 / 2
            _ -> 1 / 3
          step (Sums aa bb2 ab2 ar0 ag0 ab0 br0 bg0 bb0) (k, (r, g, b)) =
            let a = weight k
                b' = 1 - a
                (fr, fg, fb) = (fromIntegral r, fromIntegral g, fromIntegral b)
            in Sums (aa + a * a) (bb2 + b' * b') (ab2 + a * b')
                 (ar0 + a * fr) (ag0 + a * fg) (ab0 + a * fb)
                 (br0 + b' * fr) (bg0 + b' * fg) (bb0 + b' * fb)
          Sums saa sbb sab ar ag ab br bg bb = L.foldl' step (Sums 0 0 0 0 0 0 0 0 0) (zip picked points)
          det = saa * sbb - sab * sab
          sap = (ar, ag, ab)
          sbp = (br, bg, bb)
      in if abs det < 1e-9
           then rangeFit points
           else ( scale (1 / det) (sub (scale sbb sap) (scale sab sbp))
                , scale (1 / det) (sub (scale saa sbp) (scale sab sap)) )

-- | Endpoints of a colour block, the error of the pixels, and their indices.
data Fit = Fit !Int !Int !Int [Int]

fitError :: Fit -> Int
fitError (Fit _ _ err _) = err

-- | Nine running sums, for the least squares fit and the covariance.
data Sums = Sums !Double !Double !Double !Double !Double !Double !Double !Double !Double

-- | Two points spanning the pixels along their principal axis.
rangeFit :: [RGB] -> (V3, V3)
rangeFit pts = (along hi, along lo)
  where
    -- Sums of the channels and of their products, in one pass.
    moment (Sums r g b rr gg bb rg rb gb) (pr, pg, pb) =
      let (x, y, z) = (fromIntegral pr, fromIntegral pg, fromIntegral pb)
      in Sums (r + x) (g + y) (b + z) (rr + x * x) (gg + y * y) (bb + z * z) (rg + x * y) (rb + x * z) (gb + y * z)
    Sums sr sg sb srr sgg sbb srg srb sgb = L.foldl' moment (Sums 0 0 0 0 0 0 0 0 0) pts
    n = fromIntegral (length pts)
    m@(mr, mg, mb) = (sr / n, sg / n, sb / n)
    -- Covariance matrix, times n.
    cxx = srr - n * mr * mr; cyy = sgg - n * mg * mg; czz = sbb - n * mb * mb
    cxy = srg - n * mr * mg; cxz = srb - n * mr * mb; cyz = sgb - n * mg * mb
    cov (x, y, z) = (cxx * x + cxy * y + cxz * z, cxy * x + cyy * y + cyz * z, cxz * x + cyz * y + czz * z)
    power v = let v' = cov v; l = norm v' in if l < 1e-9 then v else scale (1 / l) v'
    -- Start from the pixel farthest from the mean: unlike a fixed vector, it
    -- is never orthogonal to the spread, which would collapse the fit.
    farthest = snd $ L.foldl' (\best@(bd, _) p -> let d = sub (toD p) m; l = dot d d in if l > bd then (l, d) else best) (0, (1, 1, 1)) pts
    axis = iterate power farthest !! 8
    (lo, hi) = L.foldl' (\(a, b) p -> let t = dot (sub (toD p) m) axis in (min a t, max b t)) (1 / 0, -1 / 0) pts
    along t = add m (scale t axis)

type V3 = (Double, Double, Double)

toD :: RGB -> V3
toD (r, g, b) = (fromIntegral r, fromIntegral g, fromIntegral b)

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

-- | The colours of a block as 'decodeDxt' computes them.
palette :: Bool -> Int -> Int -> [RGB]
palette four e0 e1 =
  [ (r0, g0, b0), (r1, g1, b1)
  , (mix2 r0 r1, mix2 g0 g1, mix2 b0 b1)
  , (mix3 r0 r1, mix3 g0 g1, mix3 b0 b1) ]
  where
    (r0, g0, b0) = (expand5 (e0 `shiftR` 11), expand6 (e0 `shiftR` 5), expand5 e0)
    (r1, g1, b1) = (expand5 (e1 `shiftR` 11), expand6 (e1 `shiftR` 5), expand5 e1)
    mix2 a b = if four then (2 * a + b) `quot` 3 else (a + b) `quot` 2
    mix3 a b = if four then (a + 2 * b) `quot` 3 else 0

dist :: RGB -> RGB -> Int
dist (a, b, c) (d, e, f) = (a - d) ^ (2 :: Int) + (b - e) ^ (2 :: Int) + (c - f) ^ (2 :: Int)

-- | A colour block's bytes: endpoints, then 2-bit indices; missing pixels
-- get index 0.
block :: Int -> Int -> [(Int, Int)] -> [Word8]
block e0 e1 ixs = littleEndian 2 e0 ++ littleEndian 2 e1 ++ littleEndian 4 bits
  where bits = foldr (\(i, k) acc -> acc .|. k `shiftL` (2 * i)) 0 ixs

-- | DXT3 alpha: 4 bits per pixel, rounded.
dxt3AlphaBytes :: [(Int, PixelRGBA8)] -> [Word8]
dxt3AlphaBytes px = littleEndian 8 bits
  where bits = foldr (\(i, PixelRGBA8 _ _ _ a) acc -> acc .|. min 15 ((fromIntegral a + 8) `div` 17) `shiftL` (4 * i)) 0 px

-- | DXT5 alpha: endpoints and 3-bit indices, in whichever mode has the
-- smaller squared error. The six-step mode spans the block's alphas; the
-- four-step mode spans those other than 0 and 255, which it has exactly.
dxt5AlphaBytes :: [(Int, PixelRGBA8)] -> [Word8]
dxt5AlphaBytes px = fromIntegral a0 : fromIntegral a1 : littleEndian 6 bits
  where
    alphas = [(i, fromIntegral a) | (i, PixelRGBA8 _ _ _ a) <- px]
    values = map snd alphas
    inner = [v | v <- values, v /= 0, v /= 255]
    six = (maximum values, minimum values)
    four = if null inner then (0, 0) else (minimum inner, maximum inner)
    -- The nearest table entry for every alpha, and the total error.
    assign (e0, e1) =
      let table = [(j, dxt5AlphaValue e0 e1 j) | j <- [0 .. 7]]
          nearest v = L.foldl' (\best@(bd, _) (j, t) -> let d = (v - t) * (v - t) in if d < bd then (d, j) else best) (maxBound, 0) table
          picks = map nearest values
      in (sum (map fst picks), map snd picks)
    ((a0, a1), (_, ks))
      | null values = ((0, 0), (0, []))
      | fst sixFit <= fst fourFit = (six, sixFit)
      | otherwise = (four, fourFit)
      where
        sixFit = assign six
        fourFit = assign four
    bits = foldr (\(i, k) acc -> acc .|. k `shiftL` (3 * i)) 0 (zip (map fst alphas) ks)

-- | The low n bytes of a number, least significant first.
littleEndian :: Int -> Int -> [Word8]
littleEndian n v = [fromIntegral (v `shiftR` (8 * k)) | k <- [0 .. n - 1]]

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
