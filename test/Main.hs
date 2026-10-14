module Main(main) where

import Codec.Picture
import Codec.Picture.Blp
import Codec.Picture.Types (convertImage)
import Control.Exception (bracket_, try)
import Data.ByteString (ByteString)
import Data.List (isInfixOf)
import Data.Word
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Fixture

main :: IO ()
main = defaultMain $ testGroup "JuicyPixels-blp"
  [ uncompressedTests
  , malformedTests
  , jpegTests
  , encoderTests
  , fileTests
  ]

-- | 4x4 test texture: pixel (x, y) is palette index @y*4+x@.
indices :: ByteString
indices = BS.pack [0 .. 15]

-- | Alpha plane of the with-alpha variant.
alphaPlane :: ByteString
alphaPlane = BS.pack [k * 16 + 15 | k <- [0 .. 15]]

-- | Palette stored in the file; only the first 16 entries are referenced.
palette :: Int -> [PaletteEntry]
palette n = take n $ [(k * 3, 255 - k * 16, k * 16, 0) | k <- [0 .. 15]] ++ repeat (0, 0, 0, 0)

-- | What the decoder should produce for index @k@: palette colour, alpha from
-- the alpha plane (with alpha) or opaque (without alpha).
withAlphaPixel, withoutAlphaPixel :: Word8 -> PixelRGBA8
withAlphaPixel k = PixelRGBA8 (k * 16) (255 - k * 16) (k * 3) (k * 16 + 15)
withoutAlphaPixel k = PixelRGBA8 (k * 16) (255 - k * 16) (k * 3) 255

withAlphaFile :: Word32 -> ByteString
withAlphaFile ptype = uncompressed defHeader { hPictureType = ptype } (palette 256) [indices <> alphaPlane]

withoutAlphaFile :: ByteString
withoutAlphaFile = uncompressed defHeader { hFlags = 0, hPictureType = 5 } (palette 256) [indices]

pixelsOf :: Image PixelRGBA8 -> [PixelRGBA8]
pixelsOf img = [pixelAt img x y | y <- [0 .. imageHeight img - 1], x <- [0 .. imageWidth img - 1]]

decodeRGBA :: ByteString -> IO (Image PixelRGBA8)
decodeRGBA bs = either (assertFailure . ("decodeBlp: " ++)) (return . convertRGBA8) $ decodeBlp bs

decodeLevels :: ByteString -> IO [Image PixelRGBA8]
decodeLevels bs = either (assertFailure . ("decodeBlpMipmaps: " ++)) (return . map convertRGBA8) $ decodeBlpMipmaps bs

assertPixels :: (Word8 -> PixelRGBA8) -> ByteString -> Assertion
assertPixels expected bs = do
  img <- decodeRGBA bs
  (imageWidth img, imageHeight img) @?= (4, 4)
  pixelsOf img @?= map expected [0 .. 15]

assertLeft :: String -> Either String a -> Assertion
assertLeft needle r = case r of
  Left e -> assertBool ("error " ++ show e ++ " should mention " ++ show needle) (needle `isInfixOf` e)
  Right _ -> assertFailure "expected a decoding error"

uncompressedTests :: TestTree
uncompressedTests = testGroup "uncompressed"
  [ testCase "with alpha, picture type 4" $ assertPixels withAlphaPixel (withAlphaFile 4)
  , testCase "with alpha, picture type 3" $ assertPixels withAlphaPixel (withAlphaFile 3)
  , testCase "picture type 0 decodes as with alpha (#10)" $ assertPixels withAlphaPixel (withAlphaFile 0)
  , testCase "without alpha, picture type 5" $ assertPixels withoutAlphaPixel withoutAlphaFile
  , testCase "without alpha ignores the palette's fourth byte (#14)" $
      assertPixels withoutAlphaPixel $ uncompressed defHeader { hFlags = 0, hPictureType = 5 }
        [(b, g, r, 255) | (b, g, r, _) <- palette 256] [indices]
  , testCase "labelled with alpha, mip 0 holds only indices (#8)" $
      assertPixels withoutAlphaPixel $ uncompressed defHeader (palette 256) [indices]
  , testCase "labelled with alpha, mip 0 between one and two planes (#8)" $
      assertPixels withoutAlphaPixel $ uncompressed defHeader (palette 256) [indices <> "pad"]
  , testCase "palette shorter than 256 entries (#6)" $
      assertPixels withoutAlphaPixel $ uncompressed defHeader { hFlags = 0, hPictureType = 5 } (palette 16) [indices]
  , testCase "mipmap slot past the end of the file is skipped (#7)" $ do
      let bs = setDword (offsetSlot 1) 100000 . setDword (sizeSlot 1) 16 $ withAlphaFile 4
      assertPixels withAlphaPixel bs
      levels <- decodeLevels bs
      length levels @?= 1
  , testCase "mipmap slot overlapping mip 0 is skipped (#7)" $ do
      let mip0 = getDword (offsetSlot 0) (withAlphaFile 4)
          bs = setDword (offsetSlot 1) (mip0 + 4) . setDword (sizeSlot 1) 8 $ withAlphaFile 4
      assertPixels withAlphaPixel bs
      levels <- decodeLevels bs
      length levels @?= 1
  , testCase "with alpha mip levels have their own sizes (#13)" $ do
      let bs = uncompressed defHeader (palette 256)
            [indices <> alphaPlane, "\1\2\3\4" <> "\5\6\7\8", "\9" <> "\10"]
      levels <- decodeLevels bs
      sizes levels @?= [(4, 4), (2, 2), (1, 1)]
      pixelsOf (levels !! 1) @?= [PixelRGBA8 (k * 16) (255 - k * 16) (k * 3) (k + 4) | k <- [1 .. 4]]
      pixelsOf (levels !! 2) @?= [PixelRGBA8 144 111 27 10]
  , testCase "without alpha mip levels have their own sizes (#13)" $ do
      let bs = uncompressed defHeader { hFlags = 0, hPictureType = 5 } (palette 256) [indices, "\1\2\3\4", "\9"]
      levels <- decodeLevels bs
      sizes levels @?= [(4, 4), (2, 2), (1, 1)]
      pixelsOf (levels !! 1) @?= map withoutAlphaPixel [1 .. 4]
  , testCase "non-square texture halves each side down to 1 (#13)" $ do
      let bs = uncompressed defHeader { hFlags = 0, hPictureType = 5, hWidth = 4, hHeight = 2 } (palette 256)
            [BS.take 8 indices, "\1\2", "\3"]
      levels <- decodeLevels bs
      sizes levels @?= [(4, 2), (2, 1), (1, 1)]
  , testCase "mip level shorter than its size ends the list (#13)" $ do
      let bs = uncompressed defHeader { hFlags = 0, hPictureType = 5 } (palette 256) [indices, "\1\2", "\9"]
      levels <- decodeLevels bs
      sizes levels @?= [(4, 4)]
  , testCase "mip 0 shorter than the image is an error, not an exception (#13)" $
      assertLeft "too short" . decodeBlp $ uncompressed defHeader { hFlags = 0, hPictureType = 5 } (palette 256) [BS.take 8 indices]
  ]

malformedTests :: TestTree
malformedTests = testGroup "malformed input"
  [ testCase "wrong version tag" $
      assertLeft "BLP1" . decodeBlp $ "BLP2" <> BS.drop 4 (withAlphaFile 4)
  , testCase "unknown compression" $
      assertLeft "compression" . decodeBlp $ uncompressed defHeader { hCompression = 7 } (palette 256) [indices]
  , testCase "unknown picture type" $
      assertLeft "picture type" . decodeBlp $ withAlphaFile 9
  , testCase "uncompressed file with JPEG picture type" $
      assertLeft "JPEG type" . decodeBlp $ withAlphaFile 2
  , testCase "truncated header" $
      assertLeft "" . decodeBlp $ BS.take 100 (withAlphaFile 4)
  , testCase "empty input" $
      assertLeft "" $ decodeBlp BS.empty
  , testCase "file cut inside the header names the field" $
      assertLeft "mipmaps sizes" . decodeBlp $ BS.take 100 (withAlphaFile 4)
  , testCase "JPEG header longer than the file" $
      assertLeft "jpeg header" . decodeBlp $ setDword headerSize 100000 jpegFile
  , testCase "uncompressed file without mipmaps" $
      assertLeft "No data" . decodeBlp $ blpFile defHeader [] (paletteBytes $ palette 256)
  , testCase "JPEG file without mipmaps" $
      assertLeft "No JPEG mipmap data" . decodeBlp $ blpFile defHeader { hCompression = 0 } [] "\0\0\0\0"
  ]

-- | 16x16 texture with colour and alpha gradients, enough for three mip levels.
gradient :: Image PixelRGBA8
gradient = generateImage px 16 16
  where
    px x y = PixelRGBA8 (fromIntegral $ x * 16) (fromIntegral $ y * 16) 128 (fromIntegral $ 255 - x * 8)

-- | 'gradient' with the alpha plane flattened to opaque.
opaqueGradient :: Image PixelRGBA8
opaqueGradient = pixelMap (\(PixelRGBA8 r g b _) -> PixelRGBA8 r g b 255) gradient

jpegFile :: ByteString
jpegFile = BSL.toStrict $ encodeBlpJpeg 100 3 (ImageRGBA8 gradient)

-- | Largest per-channel difference between two equally sized images.
maxDiff :: Image PixelRGBA8 -> Image PixelRGBA8 -> Int
maxDiff a b = maximum $ zipWith diff (pixelsOf a) (pixelsOf b)
  where
    diff (PixelRGBA8 r g b' x) (PixelRGBA8 r2 g2 b2 x2) =
      maximum [d r r2, d g g2, d b' b2, d x x2]
    d u v = abs (fromIntegral u - fromIntegral v :: Int)

sizes :: [Image PixelRGBA8] -> [(Int, Int)]
sizes = map (\i -> (imageWidth i, imageHeight i))

jpegTests :: TestTree
jpegTests = testGroup "jpeg"
  [ testCase "opaque texture round-trips through the encoder (#11)" $ do
      img <- decodeRGBA . BSL.toStrict $ encodeBlpJpeg 100 1 (ImageRGBA8 opaqueGradient)
      let d = maxDiff opaqueGradient img
      assertBool ("max channel difference " ++ show d) (d <= 8)
  , testCase "all three mip levels decode" $ do
      levels <- decodeLevels jpegFile
      sizes levels @?= [(16, 16), (8, 8), (4, 4)]
  , testCase "picture type 6 decodes like the original (#5)" $ do
      orig <- decodeRGBA jpegFile
      img <- decodeRGBA (setDword pictureTypeOffset 6 jpegFile)
      pixelsOf img @?= pixelsOf orig
  , testCase "broken middle mip keeps only the levels before it (#9)" $ do
      levels <- decodeLevels (setDword (sizeSlot 1) 12 jpegFile)
      sizes levels @?= [(16, 16)]
  , testCase "broken last mip keeps the levels before it (#9)" $ do
      levels <- decodeLevels (setDword (sizeSlot 2) 12 jpegFile)
      sizes levels @?= [(16, 16), (8, 8)]
  , testCase "broken mip 0 fails the file" $
      assertLeft "" . decodeBlp $ setDword (sizeSlot 0) 12 jpegFile
  , testCase "three-component JPEG decodes to RGBA" $ do
      let rgb = generateImage (\x y -> PixelRGB8 (fromIntegral $ x * 16) (fromIntegral $ y * 16) 128) 16 16
          jpeg = BSL.toStrict . encodeJpeg $ (convertImage rgb :: Image PixelYCbCr8)
          bs = blpFile defHeader { hCompression = 0, hFlags = 0, hWidth = 16, hHeight = 16, hPictureType = 2 }
            [(fromIntegral headerSize + 4, fromIntegral $ BS.length jpeg)] ("\0\0\0\0" <> jpeg)
      dyn <- either (assertFailure . ("decodeBlp: " ++)) return $ decodeBlp bs
      case dyn of
        ImageRGBA8 img -> do
          direct <- either (assertFailure . ("decodeJpeg: " ++)) (return . convertRGBA8) $ decodeJpeg jpeg
          pixelsOf img @?= pixelsOf direct
        _ -> assertFailure "expected ImageRGBA8"
  , testCase "semi-transparent texture round-trips through the encoder (#17)" $ do
      img <- decodeRGBA . BSL.toStrict $ encodeBlpJpeg 100 1 (ImageRGBA8 gradient)
      let d = maxDiff gradient img
      assertBool ("max channel difference " ++ show d) (d <= 8)
  , testCase "uniformly semi-transparent texture keeps its alpha (#17)" $ do
      let solid = generateImage (\_ _ -> PixelRGBA8 200 100 50 128) 8 8
      img <- decodeRGBA . BSL.toStrict $ encodeBlpJpeg 100 1 (ImageRGBA8 solid)
      let d = maxDiff solid img
      assertBool ("max channel difference " ++ show d) (d <= 4)
  , testCase "all-zero alpha plane means opaque (#11)" $ do
      let clear = generateImage (\_ _ -> PixelRGBA8 200 100 50 0) 8 8
          opaque = pixelMap (\(PixelRGBA8 r g b _) -> PixelRGBA8 r g b 255) clear
      img <- decodeRGBA . BSL.toStrict $ encodeBlpJpeg 100 1 (ImageRGBA8 clear)
      let d = maxDiff opaque img
      assertBool ("max channel difference " ++ show d) (d <= 4)
  , testCase "mipmaps share a JPEG header (#18)" $ do
      let shared = getDword headerSize jpegFile
      assertBool ("shared header size " ++ show shared) (shared > 0 && shared <= 624)
      levels <- decodeLevels jpegFile
      sizes levels @?= [(16, 16), (8, 8), (4, 4)]
  , testCase "single mipmap keeps its data out of the shared header (#18)" $ do
      let tiny = generateImage (\_ _ -> PixelRGBA8 200 100 50 255) 1 1
      img <- decodeRGBA . BSL.toStrict $ encodeBlpJpeg 100 1 (ImageRGBA8 tiny)
      let d = maxDiff tiny img
      assertBool ("max channel difference " ++ show d) (d <= 4)
  ]

encoderTests :: TestTree
encoderTests = testGroup "encoder"
  [ testCase "with alpha writes the expected header" $ do
      let bs = BSL.toStrict $ encodeBlpUncompressedWithAlpha 3 (ImageRGBA8 gradient)
      map (`getDword` bs) [4, 8, 12, 16] @?= [1, 8, 16, 16]
      getDword pictureTypeOffset bs @?= 3
      map (\i -> getDword (sizeSlot i) bs) [0 .. 2] @?= [512, 128, 32]
  , testCase "without alpha writes the expected header" $ do
      let bs = BSL.toStrict $ encodeBlpUncompressedWithoutAlpha 3 (ImageRGB8 $ convertRGB8 (ImageRGBA8 gradient))
      map (`getDword` bs) [4, 8] @?= [1, 0]
      getDword pictureTypeOffset bs @?= 5
      map (\i -> getDword (sizeSlot i) bs) [0 .. 2] @?= [256, 64, 16]
  , testCase "with alpha round-trips mip 0" $ do
      img <- decodeRGBA . BSL.toStrict $ encodeBlpUncompressedWithAlpha 1 (ImageRGBA8 gradient)
      let d = maxDiff gradient img
      assertBool ("max channel difference " ++ show d) (d <= 8)
  , testCase "without alpha round-trips mip 0 (#14)" $ do
      let opaque = convertRGB8 (ImageRGBA8 opaqueGradient)
      img <- decodeRGBA . BSL.toStrict $ encodeBlpUncompressedWithoutAlpha 1 (ImageRGB8 opaque)
      let d = maxDiff opaqueGradient img
      assertBool ("max channel difference " ++ show d) (d <= 8)
  , testCase "encoded mipmaps decode at their own sizes (#13)" $ do
      levels <- decodeLevels . BSL.toStrict $ encodeBlpUncompressedWithAlpha 3 (ImageRGBA8 gradient)
      sizes levels @?= [(16, 16), (8, 8), (4, 4)]
  , testCase "alpha flag and picture type follow the input format" $ do
      let header dyn = let bs = BSL.toStrict $ encodeBlpUncompressedWithAlpha 1 dyn
                       in (getDword 8 bs, getDword pictureTypeOffset bs)
          rgba = convertRGBA8 (ImageRGBA8 gradient)
      header (ImageY8 $ pixelMap (\(PixelRGBA8 r _ _ _) -> r) rgba) @?= (0, 5)
      header (ImageYA8 $ pixelMap (\(PixelRGBA8 r _ _ a) -> PixelYA8 r a) rgba) @?= (8, 3)
      header (ImageRGB16 $ pixelMap (\(PixelRGBA8 r g b _) -> PixelRGB16 (w16 r) (w16 g) (w16 b)) rgba) @?= (0, 5)
      header (ImageRGBA16 $ pixelMap (\(PixelRGBA8 r g b a) -> PixelRGBA16 (w16 r) (w16 g) (w16 b) (w16 a)) rgba) @?= (8, 3)
  , testCase "JPEG without alpha decodes opaque" $ do
      let bs = BSL.toStrict $ encodeBlpJpeg 100 1 (ImageRGB8 $ convertRGB8 (ImageRGBA8 gradient))
      getDword 8 bs @?= 0
      img <- decodeRGBA bs
      let d = maxDiff opaqueGradient img
      assertBool ("max channel difference " ++ show d) (d <= 8)
  , testCase "asking for more mipmaps than levels stops at 1x1" $ do
      levels <- decodeLevels . BSL.toStrict $ encodeBlpUncompressedWithAlpha 20 (ImageRGBA8 gradient)
      sizes levels @?= [(16, 16), (8, 8), (4, 4), (2, 2), (1, 1)]
  , testCase "non-square mip chain stops when a side reaches 1" $ do
      let wide = generateImage (\x y -> PixelRGBA8 (fromIntegral x) (fromIntegral y) 0 255) 64 16
      levels <- decodeLevels . BSL.toStrict $ encodeBlpJpeg 90 16 (ImageRGBA8 wide)
      sizes levels @?= [(64, 16), (32, 8), (16, 4), (8, 2), (4, 1)]
      odd' <- decodeLevels . BSL.toStrict $ encodeBlpUncompressedWithAlpha 16 (ImageRGBA8 $ generateImage (\_ _ -> PixelRGBA8 1 2 3 4) 5 3)
      sizes odd' @?= [(5, 3), (2, 1)]
  , testCase "mipMapsUpTo" $ do
      let img w h = ImageRGBA8 $ generateImage (\_ _ -> PixelRGBA8 0 0 0 0) w h
      mipMapsUpTo 1 (img 16 16) @?= 4
      mipMapsUpTo 4 (img 16 16) @?= 2
      mipMapsUpTo 1 (img 64 16) @?= 4
  ]

fileTests :: TestTree
fileTests = testGroup "files"
  [ testCase "write and read back each format" $ do
      tmp <- getTemporaryDirectory
      let path = tmp </> "juicypixels-blp-test.blp"
          img = ImageRGBA8 opaqueGradient
          check write = bracket_ (write path) (removeFile path) $ do
            Right single <- readBlp path
            let d = maxDiff opaqueGradient (convertRGBA8 single)
            assertBool ("max channel difference " ++ show d) (d <= 8)
            Right levels <- readBlpMipmaps path
            sizes (map convertRGBA8 levels) @?= [(16, 16), (8, 8)]
      check $ \p -> writeBlpJpeg p 100 2 img
      check $ \p -> writeBlpUncompressedWithAlpha p 2 img
      check $ \p -> writeBlpUncompressedWithoutAlpha p 2 (ImageRGB8 $ convertRGB8 img)
  , testCase "missing file is an IO error" $ do
      r <- tryIO $ readBlp "/nonexistent/juicypixels-blp.blp"
      assertBool "readBlp should throw" (either (const True) (const False) r)
  ]
  where
    tryIO :: IO a -> IO (Either IOError a)
    tryIO = try

w16 :: Word8 -> Word16
w16 v = fromIntegral v * 257
