-- | DXT block decoding: hand-built blocks with values worked out from the
-- format description, and a property comparing the decoder with a direct,
-- slow transcription of that description.
module Dxt(dxtTests) where

import Codec.Picture
import Codec.Picture.Blp.Internal.Dxt
import Fixture (colourBlock, dxt3Alpha, dxt5Alpha)
import Data.ByteString (ByteString)
import Data.Either (isLeft)
import Control.Monad (forM_)
import Data.Word
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)

import qualified Data.ByteString as BS

dxtTests :: TestTree
dxtTests = testGroup "dxt"
  [ testCase "level sizes round up to whole blocks" $ do
      dxtLevelBytes (Dxt1 False) 1 1 @?= 8
      dxtLevelBytes Dxt5 5 3 @?= 32
      dxtLevelBytes Dxt3 4 4 @?= 16
      dxtLevelBytes (Dxt1 True) 256 256 @?= 32768
  , testCase "DXT1 four-colour block: endpoints and thirds" $ do
      let img = decode1 (Dxt1 True) 4 4 $ colourBlock 0xF800 0x001F [0, 1, 2, 3]
      row0 img @?= [rgba 255 0 0 255, rgba 0 0 255 255, rgba 170 0 85 255, rgba 85 0 170 255]
  , testCase "DXT1 three-colour block: half way and black" $ do
      let block = colourBlock 0x001F 0xF800 [0, 1, 2, 3]
      row0 (decode1 (Dxt1 False) 4 4 block) @?= [rgba 0 0 255 255, rgba 255 0 0 255, rgba 127 0 127 255, rgba 0 0 0 255]
      row0 (decode1 (Dxt1 True) 4 4 block) @?= [rgba 0 0 255 255, rgba 255 0 0 255, rgba 127 0 127 255, rgba 0 0 0 0]
  , testCase "DXT1 equal endpoints use three-colour mode" $
      row0 (decode1 (Dxt1 True) 4 4 $ colourBlock 0x1234 0x1234 [3, 0]) @?= [rgba 0 0 0 0, rgba 16 69 165 255, rgba 16 69 165 255, rgba 16 69 165 255]
  , testCase "565 endpoints expand by bit replication" $
      -- r = 16, g = 32, b = 16: 16 << 3 | 16 >> 2 = 132, 32 << 2 | 32 >> 4 = 130.
      pixelAt (decode1 (Dxt1 False) 4 4 $ colourBlock 0x8410 0 []) 0 0 @?= rgba 132 130 132 255
  , testCase "DXT3: 4-bit alpha per pixel, colour always four-colour" $ do
      let img = decode1 Dxt3 4 4 $ dxt3Alpha [0 .. 15] <> colourBlock 0x001F 0xF800 (replicate 15 0 ++ [3])
      [a | y <- [0 .. 3], x <- [0 .. 3], let PixelRGBA8 _ _ _ a = pixelAt img x y] @?= [17 * n | n <- [0 .. 15]]
      pixelAt img 3 3 @?= rgba 170 0 85 255
  , testCase "DXT5 with a0 > a1: six interpolated values" $ do
      let img = decode1 Dxt5 4 4 $ dxt5Alpha 255 0 [i `mod` 8 | i <- [0 .. 15]] <> colourBlock 0 0 []
      alphas img @?= concat (replicate 2 [255, 0, 218, 182, 145, 109, 72, 36])
  , testCase "DXT5 with a0 <= a1: four interpolated values, 0 and 255" $ do
      let img = decode1 Dxt5 4 4 $ dxt5Alpha 0 255 [i `mod` 8 | i <- [0 .. 15]] <> colourBlock 0 0 []
      alphas img @?= concat (replicate 2 [0, 255, 51, 102, 153, 204, 0, 255])
  , testCase "blocks are stored row by row, partial blocks are cropped" $ do
      -- 5x6: two blocks across, two down.
      let red = colourBlock 0xF800 0 []
          green = colourBlock 0x07E0 0 []
          blue = colourBlock 0x001F 0 []
          white = colourBlock 0xFFFF 0 []
          img = decode1 (Dxt1 False) 5 6 $ BS.concat [red, green, blue, white]
          colour (PixelRGBA8 r g b _) = (r, g, b)
      (imageWidth img, imageHeight img) @?= (5, 6)
      [colour (pixelAt img x y) | (x, y) <- [(3, 3), (4, 3), (3, 4), (4, 5)]] @?= [(255, 0, 0), (0, 255, 0), (0, 0, 255), (255, 255, 255)]
  , testCase "short data and empty sizes are errors, extra bytes are ignored" $ do
      let block = colourBlock 0xF800 0 []
      assertBool "short" . isLeft $ decodeDxt (Dxt1 False) 5 1 block
      assertBool "zero width" . isLeft $ decodeDxt (Dxt1 False) 0 4 block
      fmap imageData (decodeDxt (Dxt1 False) 4 4 (block <> BS.replicate 9 0xAB))
        @?= fmap imageData (decodeDxt (Dxt1 False) 4 4 block)
  , testProperty "matches a direct transcription of the format" matchesReference
  , encoderTests
  ]

rgba :: Word8 -> Word8 -> Word8 -> Word8 -> PixelRGBA8
rgba = PixelRGBA8

decode1 :: DxtFormat -> Int -> Int -> ByteString -> Image PixelRGBA8
decode1 fmt w h = either error id . decodeDxt fmt w h

row0 :: Image PixelRGBA8 -> [PixelRGBA8]
row0 img = [pixelAt img x 0 | x <- [0 .. 3]]

alphas :: Image PixelRGBA8 -> [Word8]
alphas img = [a | y <- [0 .. 3], x <- [0 .. 3], let PixelRGBA8 _ _ _ a = pixelAt img x y]

-- | A random image in every format: random bytes of exactly the right size.
data Case = Case DxtFormat Int Int ByteString

instance Show Case where
  show (Case fmt w h bs) = show fmt ++ " " ++ show w ++ "x" ++ show h ++ " " ++ show (BS.unpack bs)

instance Arbitrary Case where
  arbitrary = do
    fmt <- elements [Dxt1 False, Dxt1 True, Dxt3, Dxt5]
    w <- choose (1, 13)
    h <- choose (1, 13)
    -- Few distinct bytes make equal endpoints and edge cases common.
    bytes <- vectorOf (dxtLevelBytes fmt w h) (oneof [arbitrary, elements [0, 0xFF, 0x80, 0x7F]])
    return $ Case fmt w h (BS.pack bytes)

matchesReference :: Case -> Property
matchesReference (Case fmt w h bs) =
  fmap (\img -> [pixelAt img x y | y <- [0 .. h - 1], x <- [0 .. w - 1]]) (decodeDxt fmt w h bs)
    === Right (reference fmt w h bs)

-- | The format description, transcribed pixel by pixel with lists and
-- integer arithmetic.
reference :: DxtFormat -> Int -> Int -> ByteString -> [PixelRGBA8]
reference fmt w h bs = [pixel x y | y <- [0 .. h - 1], x <- [0 .. w - 1]]
  where
    size = case fmt of
      Dxt1 _ -> 8
      _ -> 16
    number = foldr (\b acc -> acc * 256 + toInteger b) 0 . BS.unpack
    pixel x y = PixelRGBA8 (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)
      where
        start = ((y `div` 4) * ((w + 3) `div` 4) + x `div` 4) * size
        block = BS.take size (BS.drop start bs)
        (alphaPart, colourPart) = case fmt of
          Dxt1 _ -> (BS.empty, block)
          _ -> BS.splitAt 8 block
        i = 4 * (y `mod` 4) + x `mod` 4
        c0 = number (BS.take 2 colourPart)
        c1 = number (BS.take 2 (BS.drop 2 colourPart))
        k = fromInteger $ (number (BS.drop 4 colourPart) `div` (4 ^ i)) `mod` 4
        rgb c = (5 `bitsAt` 11, 6 `bitsAt` 5, 5 `bitsAt` 0)
          where
            bitsAt n from = let v = fromInteger ((c `div` (2 ^ (from :: Int))) `mod` (2 ^ (n :: Int))) :: Int
                            in v * 2 ^ (8 - n) + v `div` 2 ^ (2 * n - 8)
        (p0, p1) = (rgb c0, rgb c1)
        four = case fmt of
          Dxt1 _ -> c0 > c1
          _ -> True
        mixed m n = let f u v = (m * u + n * v) `div` (m + n) in zip3' f p0 p1
        zip3' f (u0, v0, w0) (u1, v1, w1) = (f u0 u1, f v0 v1, f w0 w1)
        colours
          | four = [p0, p1, mixed 2 1, mixed 1 2]
          | otherwise = [p0, p1, mixed 1 1, (0, 0, 0)]
        (r, g, b) = colours !! k
        a = case fmt of
          Dxt1 True | not four && k == 3 -> 0
          Dxt1 _ -> 255
          Dxt3 -> 17 * fromInteger ((number alphaPart `div` (16 ^ i)) `mod` 16)
          Dxt5 -> table !! fromInteger ((number (BS.drop 2 alphaPart) `div` (8 ^ i)) `mod` 8)
        a0 = fromIntegral (BS.index alphaPart 0) :: Int
        a1 = fromIntegral (BS.index alphaPart 1)
        table
          | a0 > a1 = [a0, a1] ++ [((7 - j) * a0 + j * a1) `div` 7 | j <- [1 .. 6]]
          | otherwise = [a0, a1] ++ [((5 - j) * a0 + j * a1) `div` 5 | j <- [1 .. 4]] ++ [0, 255]

encoderTests :: TestTree
encoderTests = testGroup "encoding"
  [ testCase "output is whole blocks and decodes to the same size" $
      forM_ [Dxt1 False, Dxt1 True, Dxt3, Dxt5] $ \fmt -> do
        let img = generateImage (\x y -> PixelRGBA8 (fromIntegral $ x * 40) (fromIntegral $ y * 70) 99 200) 5 3
            bytes = encodeDxt fmt img
        BS.length bytes @?= dxtLevelBytes fmt 5 3
        fmap (\d -> (imageWidth d, imageHeight d)) (decodeDxt fmt 5 3 bytes) @?= Right (5, 3)
  , testProperty "blocks of at most two RGB565 colours round-trip exactly" twoColours
  , testProperty "a block of one colour is within 1 per channel" oneColour
  , testProperty "DXT1 with alpha: below 128 is transparent, the rest opaque" dxt1Alpha
  , testProperty "DXT3 alpha is the nearest multiple of 17" dxt3Rounding
  , testProperty "DXT5 alpha with two values per block is exact" dxt5TwoAlphas
  , testCase "DXT5 uses the mode with 0 and 255 when it fits better" $ do
      let img = generateImage (\x y -> PixelRGBA8 9 9 9 ([0, 100, 110, 255] !! ((x + y) `mod` 4))) 4 4
          alpha (PixelRGBA8 _ _ _ a) = a
      map alpha (pixels (roundTrip Dxt5 img)) @?= map alpha (pixels img)
  , testCase "two colours whose difference is orthogonal to grey round-trip" $ do
      -- (16, -16, 0) is orthogonal to (1, 1, 1); a principal axis search
      -- started from (1, 1, 1) collapsed here to the mean colour.
      let img = generateImage (\x y -> if even (x + y) then PixelRGBA8 239 239 41 255 else PixelRGBA8 255 223 41 255) 4 4
      pixels (roundTrip (Dxt1 False) img) @?= pixels img
  , testCase "endpoints are refined by least squares" $ do
      -- One black, fourteen mid-grey and one white pixel: the range fit puts
      -- the endpoints at black and white, which no index decodes near the
      -- grey. The squared error was 12802 when this was written; without
      -- refinement it is 19851.
      let grey i = case i of { 0 -> 0; 15 -> 255; _ -> 128 } :: Word8
          img = generateImage (\x y -> let v = grey (4 * y + x) in PixelRGBA8 v v v 255) 4 4
          chans (PixelRGBA8 r g b _) = map fromIntegral [r, g, b] :: [Int]
          err = sum [(a - b) ^ (2 :: Int) | (p, q) <- zip (pixels img) (pixels (roundTrip (Dxt1 False) img)), (a, b) <- zip (chans p) (chans q)]
      assertBool ("squared error " ++ show err) (err < 15000)
  , testCase "a smooth gradient keeps its quality" $ do
      -- 44.0 dB (DXT1) and 45.3 dB (DXT5, with alpha) when this was written.
      let img = generateImage (\x y -> PixelRGBA8 (fromIntegral x) (fromIntegral (y * 2)) (fromIntegral (x + y)) (fromIntegral (255 - y))) 256 128
          quality fmt alpha = psnr alpha img (roundTrip fmt img)
      assertBool "DXT1" (quality (Dxt1 False) False > 42)
      assertBool "DXT5" (quality Dxt5 True > 42)
  ]

roundTrip :: DxtFormat -> Image PixelRGBA8 -> Image PixelRGBA8
roundTrip fmt img = either error id $ decodeDxt fmt (imageWidth img) (imageHeight img) (encodeDxt fmt img)

pixels :: Image PixelRGBA8 -> [PixelRGBA8]
pixels img = [pixelAt img x y | y <- [0 .. imageHeight img - 1], x <- [0 .. imageWidth img - 1]]

psnr :: Bool -> Image PixelRGBA8 -> Image PixelRGBA8 -> Double
psnr alpha a b = 10 * logBase 10 (255 * 255 / (fromIntegral (sum errs) / fromIntegral (length errs)))
  where
    chans (PixelRGBA8 r g bl al) = map fromIntegral ([r, g, bl] ++ [al | alpha]) :: [Int]
    errs = [(x - y) ^ (2 :: Int) | (p, q) <- zip (pixels a) (pixels b), (x, y) <- zip (chans p) (chans q)]

-- | A random image of w x h, each pixel from the given generator.
blocky :: Int -> Int -> (Int -> Int -> Gen PixelRGBA8) -> Gen (Image PixelRGBA8)
blocky w h pixelFor = do
  rows <- sequence [sequence [pixelFor x y | x <- [0 .. w - 1]] | y <- [0 .. h - 1]]
  return $ generateImage (\x y -> rows !! y !! x) w h


-- | One random value per 4x4 block of a w x h image, looked up by pixel.
perBlock :: Int -> Int -> Gen a -> Gen (Int -> Int -> a)
perBlock w h gen = do
  let across = (w + 3) `div` 4
  values <- vectorOf (across * ((h + 3) `div` 4)) gen
  return $ \x y -> values !! ((y `div` 4) * across + x `div` 4)
-- | A colour whose RGB565 form expands back to itself.
exact565 :: Gen (Word8, Word8, Word8)
exact565 = do
  r <- choose (0, 31 :: Int)
  g <- choose (0, 63 :: Int)
  b <- choose (0, 31 :: Int)
  let e5 v = fromIntegral (v * 8 + v `div` 4)
      e6 v = fromIntegral (v * 4 + v `div` 16)
  return (e5 r, e6 g, e5 b)

sizes :: Gen (Int, Int)
sizes = (,) <$> choose (1, 13) <*> choose (1, 13)

twoColours :: Property
twoColours = forAll sizes $ \(w, h) -> forAll (elements [Dxt1 False, Dxt3, Dxt5]) $ \fmt -> do
  pairOf <- perBlock w h ((,) <$> exact565 <*> exact565)
  img <- blocky w h $ \x y -> do
    let (c1, c2) = pairOf x y
    (r, g, b) <- elements [c1, c2]
    return (PixelRGBA8 r g b 255)
  let rgb (PixelRGBA8 r g b _) = (r, g, b)
  return $ map rgb (pixels (roundTrip fmt img)) === map rgb (pixels img)

oneColour :: Property
oneColour = forAll (elements [Dxt1 False, Dxt1 True, Dxt3, Dxt5]) $ \fmt ->
  forAll ((,,) <$> arbitrary <*> arbitrary <*> arbitrary) $ \(r, g, b) ->
    let img = generateImage (\_ _ -> PixelRGBA8 r g b 255) 4 4
        close a a' = abs (fromIntegral a - fromIntegral a' :: Int) <= 1
    in conjoin [ counterexample (show p) (close r r' && close g g' && close b b')
               | p@(PixelRGBA8 r' g' b' _) <- pixels (roundTrip fmt img) ]

dxt1Alpha :: Property
dxt1Alpha = forAll sizes $ \(w, h) -> do
  img <- blocky w h $ \_ _ -> PixelRGBA8 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  let alpha (PixelRGBA8 _ _ _ a) = a
  return $ map alpha (pixels (roundTrip (Dxt1 True) img)) === [if alpha p < 128 then 0 else 255 | p <- pixels img]

dxt3Rounding :: Property
dxt3Rounding = forAll sizes $ \(w, h) -> do
  img <- blocky w h $ \_ _ -> PixelRGBA8 0 0 0 <$> arbitrary
  let alpha (PixelRGBA8 _ _ _ a) = fromIntegral a :: Int
  return $ map alpha (pixels (roundTrip Dxt3 img)) === [17 * ((alpha p + 8) `div` 17) | p <- pixels img]

dxt5TwoAlphas :: Property
dxt5TwoAlphas = forAll sizes $ \(w, h) -> do
  pairOf <- perBlock w h ((,) <$> arbitrary <*> arbitrary)
  img <- blocky w h $ \x y -> PixelRGBA8 9 9 9 <$> (let (a1, a2) = pairOf x y in elements [a1, a2])
  let alpha (PixelRGBA8 _ _ _ a) = a
  return $ map alpha (pixels (roundTrip Dxt5 img)) === map alpha (pixels img)
