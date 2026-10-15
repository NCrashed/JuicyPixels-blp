module Main(main) where

import Codec.Picture
import Codec.Picture.Blp
import Codec.Picture.Blp.Internal.Encoder (createMipMaps)
import Codec.Picture.Types (convertImage)
import Control.Exception (bracket_, try)
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.List (isInfixOf)
import Data.Word
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Deprecated
import Fixture

main :: IO ()
main = defaultMain $ testGroup "JuicyPixels-blp"
  [ uncompressedTests
  , malformedTests
  , jpegTests
  , encoderTests
  , fileTests
  , deprecatedTests
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
  , testCase "alphaBits 8 keeps alpha whatever extra says (#25)" $
      assertPixels withAlphaPixel $ uncompressed defHeader { hPictureType = 5 } (palette 256) [indices <> alphaPlane]
  , testCase "extra has no effect (#25)" $
      forM_ [0, 1, 2, 3, 4, 5, 6, 7, 99] $ \extra ->
        assertPixels withAlphaPixel (withAlphaFile extra)
  , testCase "alphaBits 0 ignores a trailing alpha list (#25)" $
      assertPixels withoutAlphaPixel $ uncompressed defHeader { hFlags = 0, hPictureType = 4 } (palette 256) [indices <> alphaPlane]
  , testCase "1-bit alpha, least significant bit first (#25)" $ do
      let bs = uncompressed defHeader { hFlags = 1 } (palette 256) [indices <> BS.pack [0x0f, 0xf0]]
          on k = k < 4 || k >= 12
      assertPixels (\k -> setAlpha (if on k then 255 else 0) (withoutAlphaPixel k)) bs
  , testCase "4-bit alpha, low nibble first (#25)" $ do
      let nibbles = BS.pack [fromIntegral (2 * j) + 16 * fromIntegral (2 * j + 1) | j <- [0 .. 7 :: Int]]
          bs = uncompressed defHeader { hFlags = 4 } (palette 256) [indices <> nibbles]
      assertPixels (\k -> setAlpha (k * 17) (withoutAlphaPixel k)) bs
  , testCase "invalid alphaBits counts as no alpha (#25)" $
      assertPixels withoutAlphaPixel $ uncompressed defHeader { hFlags = 3 } (palette 256) [indices <> alphaPlane]
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
  , testCase "truncated header" $
      assertLeft "" . decodeBlp $ BS.take 100 (withAlphaFile 4)
  , testCase "empty input" $
      assertLeft "" $ decodeBlp BS.empty
  , testCase "file cut inside the header names the field" $
      assertLeft "mipmaps sizes" . decodeBlp $ BS.take 100 (withAlphaFile 4)
  , testCase "JPEG header longer than the file" $
      assertLeft "jpeg header" . decodeBlp $ setDword headerSize 100000 jpegFile
  , testCase "width or height outside 1..65535 is an error" $ do
      let sized w h = uncompressed defHeader { hFlags = 0, hWidth = w, hHeight = h } (palette 256) [BS.replicate 4 0]
      assertLeft "Invalid image size" . decodeBlp $ sized 0xFFFFFFFF 0xFFFFFFFF
      assertLeft "Invalid image size" . decodeBlp $ sized 0 4
      assertLeft "Invalid image size" . decodeBlp $ sized 4 65536
  , testCase "garbage slot before mip 0 leaves the palette intact" $ do
      let bs = setDword (offsetSlot 1) 200 . setDword (sizeSlot 1) 4 $ withAlphaFile 4
      assertPixels withAlphaPixel bs
      levels <- decodeLevels bs
      length levels @?= 1
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
jpegFile = BSL.toStrict $ enc (BlpFormatJpeg 100) 3 (ImageRGBA8 gradient)

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
      img <- decodeRGBA . BSL.toStrict $ enc (BlpFormatJpeg 100) 1 (ImageRGBA8 opaqueGradient)
      let d = maxDiff opaqueGradient img
      assertBool ("max channel difference " ++ show d) (d <= 8)
  , testCase "all three mip levels decode" $ do
      levels <- decodeLevels jpegFile
      sizes levels @?= [(16, 16), (8, 8), (4, 4)]
  , testCase "JPEG with alphaBits 0 decodes opaque (#25)" $ do
      img <- decodeRGBA (setDword 8 0 jpegFile)
      assertBool "every pixel opaque" $ all (\(PixelRGBA8 _ _ _ a) -> a == 255) (pixelsOf img)
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
  , testCase "JPEG frame larger than its level is rejected before decoding" $ do
      let sof = frameHeaderAt (fromIntegral headerSize + 4) jpegFile
          huge = foldr (\(i, v) bs -> setByte i v bs) jpegFile [(sof + k, 0xFF) | k <- [5 .. 8]]
      assertLeft "larger than its level size" $ decodeBlp huge
  , testCase "JPEG frame smaller than its level is padded with transparent black" $ do
      let big = setDword 12 32 (setDword 16 32 jpegFile)
      levels <- decodeLevels big
      sizes (take 2 levels) @?= [(32, 32), (16, 16)]
      orig <- decodeRGBA jpegFile
      case levels of
        mip0 : _ -> do
          pixelAt mip0 3 5 @?= pixelAt orig 3 5
          pixelAt mip0 20 20 @?= PixelRGBA8 0 0 0 0
        [] -> assertFailure "no levels"
  , testCase "a missing middle slot ends the chain" $ do
      levels <- decodeLevels (setDword (sizeSlot 1) 0 jpegFile)
      sizes levels @?= [(16, 16)]
  , testCase "a JPEG decoder error in mip 0 is a Left" $
      assertLeft "JPEG decoding failed" . decodeBlp $ jpegBlp 1 1 [badJpeg]
  , testCase "a JPEG decoder error in a tail mip ends the list" $ do
      let good = BSL.toStrict . encodeJpeg $ (convertImage (generateImage (\_ _ -> PixelRGB8 10 20 30) 2 2 :: Image PixelRGB8) :: Image PixelYCbCr8)
      levels <- decodeLevels (jpegBlp 2 2 [good, badJpeg])
      sizes levels @?= [(2, 2)]
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
      img <- decodeRGBA . BSL.toStrict $ enc (BlpFormatJpeg 100) 1 (ImageRGBA8 gradient)
      let d = maxDiff gradient img
      assertBool ("max channel difference " ++ show d) (d <= 8)
  , testCase "uniformly semi-transparent texture keeps its alpha (#17)" $ do
      let solid = generateImage (\_ _ -> PixelRGBA8 200 100 50 128) 8 8
      img <- decodeRGBA . BSL.toStrict $ enc (BlpFormatJpeg 100) 1 (ImageRGBA8 solid)
      let d = maxDiff solid img
      assertBool ("max channel difference " ++ show d) (d <= 4)
  , testCase "all-zero alpha plane means opaque (#11)" $ do
      let clear = generateImage (\_ _ -> PixelRGBA8 200 100 50 0) 8 8
          opaque = pixelMap (\(PixelRGBA8 r g b _) -> PixelRGBA8 r g b 255) clear
      img <- decodeRGBA . BSL.toStrict $ enc (BlpFormatJpeg 100) 1 (ImageRGBA8 clear)
      let d = maxDiff opaque img
      assertBool ("max channel difference " ++ show d) (d <= 4)
  , testCase "mipmaps share a JPEG header (#18)" $ do
      let shared = getDword headerSize jpegFile
      assertBool ("shared header size " ++ show shared) (shared > 0 && shared <= 624)
      levels <- decodeLevels jpegFile
      sizes levels @?= [(16, 16), (8, 8), (4, 4)]
  , testCase "single mipmap keeps its data out of the shared header (#18)" $ do
      let tiny = generateImage (\_ _ -> PixelRGBA8 200 100 50 255) 1 1
      img <- decodeRGBA . BSL.toStrict $ enc (BlpFormatJpeg 100) 1 (ImageRGBA8 tiny)
      let d = maxDiff tiny img
      assertBool ("max channel difference " ++ show d) (d <= 4)
  ]

encoderTests :: TestTree
encoderTests = testGroup "encoder"
  [ testCase "with alpha writes the expected header" $ do
      let bs = BSL.toStrict $ enc (BlpFormatPaletted True) 3 (ImageRGBA8 gradient)
      map (`getDword` bs) [4, 8, 12, 16] @?= [1, 8, 16, 16]
      getDword pictureTypeOffset bs @?= 3
      map (\i -> getDword (sizeSlot i) bs) [0 .. 2] @?= [512, 128, 32]
  , testCase "without alpha writes the expected header" $ do
      let bs = BSL.toStrict $ enc (BlpFormatPaletted False) 3 (ImageRGB8 $ convertRGB8 (ImageRGBA8 gradient))
      map (`getDword` bs) [4, 8] @?= [1, 0]
      getDword pictureTypeOffset bs @?= 5
      map (\i -> getDword (sizeSlot i) bs) [0 .. 2] @?= [256, 64, 16]
  , testCase "with alpha round-trips mip 0" $ do
      img <- decodeRGBA . BSL.toStrict $ enc (BlpFormatPaletted True) 1 (ImageRGBA8 gradient)
      let d = maxDiff gradient img
      assertBool ("max channel difference " ++ show d) (d <= 8)
  , testCase "without alpha round-trips mip 0 (#14)" $ do
      let opaque = convertRGB8 (ImageRGBA8 opaqueGradient)
      img <- decodeRGBA . BSL.toStrict $ enc (BlpFormatPaletted False) 1 (ImageRGB8 opaque)
      let d = maxDiff opaqueGradient img
      assertBool ("max channel difference " ++ show d) (d <= 8)
  , testCase "encoded mipmaps decode at their own sizes (#13)" $ do
      levels <- decodeLevels . BSL.toStrict $ enc (BlpFormatPaletted True) 3 (ImageRGBA8 gradient)
      sizes levels @?= [(16, 16), (8, 8), (4, 4)]
  , testCase "JPEG alpha flag and picture type follow the input format" $ do
      let header dyn = let bs = BSL.toStrict $ enc (BlpFormatJpeg 90) 1 dyn
                       in (getDword 8 bs, getDword pictureTypeOffset bs)
          rgba = convertRGBA8 (ImageRGBA8 gradient)
      header (ImageY8 $ pixelMap (\(PixelRGBA8 r _ _ _) -> r) rgba) @?= (0, 5)
      header (ImageYA8 $ pixelMap (\(PixelRGBA8 r _ _ a) -> PixelYA8 r a) rgba) @?= (8, 3)
      header (ImageRGB16 $ pixelMap (\(PixelRGBA8 r g b _) -> PixelRGB16 (w16 r) (w16 g) (w16 b)) rgba) @?= (0, 5)
      header (ImageRGBA16 $ pixelMap (\(PixelRGBA8 r g b a) -> PixelRGBA16 (w16 r) (w16 g) (w16 b) (w16 a)) rgba) @?= (8, 3)
  , testCase "without alpha drops the alpha of an RGBA input" $ do
      let bs = BSL.toStrict $ enc (BlpFormatPaletted False) 1 (ImageRGBA8 gradient)
      (getDword 8 bs, getDword pictureTypeOffset bs) @?= (0, 5)
      img <- decodeRGBA bs
      let d = maxDiff opaqueGradient img
      assertBool ("max channel difference " ++ show d) (d <= 8)
  , testCase "with alpha keeps an alpha plane for an RGB input" $ do
      let bs = BSL.toStrict $ enc (BlpFormatPaletted True) 1 (ImageRGB8 $ convertRGB8 (ImageRGBA8 opaqueGradient))
      (getDword 8 bs, getDword pictureTypeOffset bs) @?= (8, 3)
      img <- decodeRGBA bs
      let d = maxDiff opaqueGradient img
      assertBool ("max channel difference " ++ show d) (d <= 8)
  , testCase "JPEG without alpha decodes opaque" $ do
      let bs = BSL.toStrict $ enc (BlpFormatJpeg 100) 1 (ImageRGB8 $ convertRGB8 (ImageRGBA8 gradient))
      getDword 8 bs @?= 0
      img <- decodeRGBA bs
      let d = maxDiff opaqueGradient img
      assertBool ("max channel difference " ++ show d) (d <= 8)
  , testCase "asking for more mipmaps than levels stops at 1x1" $ do
      levels <- decodeLevels . BSL.toStrict $ enc (BlpFormatPaletted True) 20 (ImageRGBA8 gradient)
      sizes levels @?= [(16, 16), (8, 8), (4, 4), (2, 2), (1, 1)]
  , testCase "non-square mip chain goes on to 1x1 (#26)" $ do
      let wide = generateImage (\x y -> PixelRGBA8 (fromIntegral x) (fromIntegral y) 0 255) 64 16
      levels <- decodeLevels . BSL.toStrict $ enc (BlpFormatJpeg 90) 16 (ImageRGBA8 wide)
      sizes levels @?= [(64, 16), (32, 8), (16, 4), (8, 2), (4, 1), (2, 1), (1, 1)]
      odd' <- decodeLevels . BSL.toStrict $ enc (BlpFormatPaletted True) 16 (ImageRGBA8 $ generateImage (\_ _ -> PixelRGBA8 1 2 3 4) 5 3)
      sizes odd' @?= [(5, 3), (2, 1), (1, 1)]
  , testCase "single-row texture keeps halving its width (#26)" $ do
      let row = ImageRGBA8 $ generateImage (\x _ -> PixelRGBA8 (fromIntegral x) 0 0 255) 8 1
      forM_ [enc (BlpFormatJpeg 90) 16 row, enc (BlpFormatPaletted True) 16 row] $ \bs -> do
        levels <- decodeLevels (BSL.toStrict bs)
        sizes levels @?= [(8, 1), (4, 1), (2, 1), (1, 1)]
  , testCase "a solid texture stays solid down to 1x1 (#26)" $ do
      let solid = PixelRGBA8 200 100 50 128
          img = ImageRGBA8 $ generateImage (\_ _ -> solid) 6 3
      levels <- decodeLevels . BSL.toStrict $ enc (BlpFormatPaletted True) 16 img
      sizes levels @?= [(6, 3), (3, 1), (1, 1)]
      forM_ levels $ \level -> assertBool "every pixel unchanged" (all (== solid) (pixelsOf level))
  , testCase "slots repeating the last level add no levels" $ do
      let bs = BSL.toStrict $ enc (BlpFormatPaletted True) 5 (ImageRGBA8 gradient)
          lastSlot = (getDword (offsetSlot 4) bs, getDword (sizeSlot 4) bs)
          padded = foldr (\i b -> setDword (offsetSlot i) (fst lastSlot) (setDword (sizeSlot i) (snd lastSlot) b)) bs [5 .. 15]
      levels <- decodeLevels padded
      sizes levels @?= [(16, 16), (8, 8), (4, 4), (2, 2), (1, 1)]
  , testCase "unused mipmap slots are zero" $ do
      let bs = BSL.toStrict $ enc (BlpFormatPaletted True) 2 (ImageRGBA8 gradient)
      [(getDword (offsetSlot i) bs, getDword (sizeSlot i) bs) | i <- [2 .. 15]] @?= replicate 14 (0, 0)
  , testCase "hasMipmaps is 0 for a single level" $ do
      let hasMipmaps n = getDword 24 . BSL.toStrict $ enc (BlpFormatJpeg 90) n (ImageRGBA8 gradient)
      hasMipmaps 1 @?= 0
      hasMipmaps 3 @?= 5
  , testCase "encodeBlpWith rejects what it cannot encode" $ do
      let opts f = BlpEncodeOptions f 16
          blank w h = ImageRGBA8 $ generateImage (\_ _ -> PixelRGBA8 0 0 0 0) w h
          errorOf o img = either Just (const Nothing) (encodeBlpWith o img)
      errorOf (opts (BlpFormatJpeg 90)) (blank 0 4) @?= Just BlpEmptyImage
      errorOf (opts (BlpFormatPaletted True)) (blank 4 0) @?= Just BlpEmptyImage
      errorOf (opts (BlpFormatJpeg 90)) (blank 65536 1) @?= Just (BlpImageTooLarge 65536 1)
      errorOf (opts (BlpFormatJpeg 0)) (blank 4 4) @?= Just (BlpInvalidJpegQuality 0)
      errorOf (opts (BlpFormatJpeg 101)) (blank 4 4) @?= Just (BlpInvalidJpegQuality 101)
      errorOf defaultBlpEncodeOptions (blank 4 4) @?= Nothing
  , testCase "colour of transparent pixels does not bleed into mipmaps" $ do
      let px x y = if (x, y) == (0, 0) then PixelRGBA8 255 255 255 255 else PixelRGBA8 0 0 0 0
          img = generateImage px 2 2
      case createMipMaps img of
        _ : level1 : _ -> pixelAt level1 0 0 @?= PixelRGBA8 255 255 255 64
        _ -> assertFailure "expected two levels"
  , testCase "paletted alpha mipmaps are averaged" $ do
      let checker = generateImage (\x y -> PixelRGBA8 200 100 50 (if even (x + y) then 0 else 255)) 16 16
      levels <- decodeLevels . BSL.toStrict $ enc (BlpFormatPaletted True) 2 (ImageRGBA8 checker)
      case levels of
        _ : level1 : _ -> assertBool "alpha around half" $
          all (\(PixelRGBA8 _ _ _ a) -> a == 128) (pixelsOf level1)
        _ -> assertFailure "expected two levels"
  , testCase "opaque mipmaps average plainly" $ do
      let img = generateImage (\x _ -> PixelRGBA8 (fromIntegral $ 10 * x) 7 0 255) 4 1
      case createMipMaps img of
        _ : level1 : _ -> pixelsOf level1 @?= [PixelRGBA8 5 7 0 255, PixelRGBA8 25 7 0 255]
        _ -> assertFailure "expected two levels"
  , testCase "mipMapsUpTo is at most 16" $
      mipMapsUpTo 1 (ImageRGBA8 $ generateImage (\_ _ -> PixelRGBA8 0 0 0 0) 65535 1) @?= 16
  , testCase "mipMapsUpTo counts levels by the larger side (#26)" $ do
      let img w h = ImageRGBA8 $ generateImage (\_ _ -> PixelRGBA8 0 0 0 0) w h
      mipMapsUpTo 1 (img 16 16) @?= 5
      mipMapsUpTo 4 (img 16 16) @?= 3
      mipMapsUpTo 1 (img 64 16) @?= 7
      mipMapsUpTo 16 (img 64 16) @?= 3
      mipMapsUpTo 1 (img 5 5) @?= 3
      mipMapsUpTo 1 (img 1 1) @?= 1
      mipMapsUpTo 32 (img 16 16) @?= 1
  , testCase "zero or negative mipmap count still writes mip 0" $ do
      let encs n = [ enc (BlpFormatJpeg 90) n (ImageRGBA8 gradient)
                   , enc (BlpFormatPaletted True) n (ImageRGBA8 gradient) ]
      forM_ (concatMap encs [0, -1]) $ \bs -> do
        levels <- decodeLevels (BSL.toStrict bs)
        sizes levels @?= [(16, 16)]
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
      check $ \p -> writeOk p (BlpFormatJpeg 100) img
      check $ \p -> writeOk p (BlpFormatPaletted True) img
      check $ \p -> writeOk p (BlpFormatPaletted False) (ImageRGB8 $ convertRGB8 img)
  , testCase "missing file is an IO error" $ do
      r <- tryIO $ readBlp "/nonexistent/juicypixels-blp.blp"
      assertBool "readBlp should throw" (either (const True) (const False) r)
  ]
  where
    tryIO :: IO a -> IO (Either IOError a)
    tryIO = try

w16 :: Word8 -> Word16
w16 v = fromIntegral v * 257

setAlpha :: Word8 -> PixelRGBA8 -> PixelRGBA8
setAlpha a (PixelRGBA8 r g b _) = PixelRGBA8 r g b a

setByte :: Int -> Word8 -> ByteString -> ByteString
setByte i v bs = BS.take i bs <> BS.singleton v <> BS.drop (i + 1) bs

-- | Offset of the first JPEG frame header (FF C0) at or after the given offset.
frameHeaderAt :: Word32 -> ByteString -> Int
frameHeaderAt from bs = fromIntegral from + BS.length (fst (BS.breakSubstring (BS.pack [0xFF, 0xC0]) (BS.drop (fromIntegral from) bs)))

-- | A 1x1 JPEG whose scan selects a component the frame does not have.
-- JuicyPixels 3.3.9 raises an error on it instead of returning Left.
badJpeg :: ByteString
badJpeg = BS.pack
  [ 0xff, 0xd8, 0xff, 0xc0, 0x00, 0x0b, 0x08, 0x00, 0x01, 0x00, 0x01, 0x01, 0x01, 0x11, 0x00
  , 0xff, 0xda, 0x00, 0x08, 0x01, 0x02, 0x00, 0x00, 0x3f, 0x00, 0xff, 0xd9 ]

-- | JPEG BLP without alpha and with an empty shared header, holding the given
-- streams as its mip levels.
jpegBlp :: Word32 -> Word32 -> [ByteString] -> ByteString
jpegBlp w h mips = blpFile defHeader { hCompression = 0, hFlags = 0, hWidth = w, hHeight = h } slots ("\0\0\0\0" <> BS.concat mips)
  where
    sizes' = map (fromIntegral . BS.length) mips
    slots = zip (scanl (+) (fromIntegral headerSize + 4) sizes') sizes'

-- | Encode with the given format and number of levels, failing the test on
-- an encoding error.
enc :: BlpFormat -> Int -> DynamicImage -> BSL.ByteString
enc format levels = either (error . show) id . encodeBlpWith (BlpEncodeOptions format levels)

-- | Write a file with two mip levels, failing the test on an encoding error.
writeOk :: FilePath -> BlpFormat -> DynamicImage -> IO ()
writeOk path format img = writeBlpWith path (BlpEncodeOptions format 2) img >>= either (assertFailure . show) return
