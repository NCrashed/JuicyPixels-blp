module Main(main) where

import Codec.Picture
import Codec.Picture.Blp
import Data.ByteString (ByteString)
import Data.List (isInfixOf)
import Data.Word
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
  , testCase "mipMapsUpTo" $ do
      let img w h = ImageRGBA8 $ generateImage (\_ _ -> PixelRGBA8 0 0 0 0) w h
      mipMapsUpTo 1 (img 16 16) @?= 4
      mipMapsUpTo 4 (img 16 16) @?= 2
      mipMapsUpTo 1 (img 64 16) @?= 4
  ]
