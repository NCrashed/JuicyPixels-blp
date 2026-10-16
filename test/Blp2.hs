-- | BLP2 files, built byte by byte: every content type, alpha depths, and
-- damaged or unsupported files.
module Blp2(blp2Tests, blp2Seeds) where

import Codec.Picture
import Codec.Picture.Blp
import Data.ByteString (ByteString)
import Data.List (isInfixOf)
import Data.Word
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString as BS

import Fixture

blp2Tests :: TestTree
blp2Tests = testGroup "blp2"
  [ testCase "DXT1: every level, block by block" $ do
      levels <- decoded dxt1File
      map size levels @?= [(8, 8), (4, 4), (2, 2), (1, 1)]
      let corners = [pixelAt (level0 levels) x y | (x, y) <- [(0, 0), (7, 0), (0, 7), (7, 7)]]
      corners @?= [red, green, blue, white]
      [pixelAt l 0 0 | l <- drop 1 levels] @?= [white, green, blue]
  , testCase "blpInfo reads the BLP2 header" $ do
      blpInfo dxt1File @?= Right (BlpInfo Blp2 BlpCompressionDxt1 8 8 0 4)
      fmap blpInfoCompression (blpInfo dxt3File) @?= Right BlpCompressionDxt3
      fmap blpInfoCompression (blpInfo dxt5File) @?= Right BlpCompressionDxt5
      blpInfo (bgraFile 8) @?= Right (BlpInfo Blp2 BlpCompressionBgra 2 1 8 2)
      blpInfo (palettedFile 8) @?= Right (BlpInfo Blp2 BlpCompressionUncompressed 2 2 8 1)
  , testCase "DXT1 with alpha depth 1: index 3 of a three-colour block is transparent" $ do
      let file depth = blp2 defHeader2 { h2AlphaDepth = depth } blankPalette [colourBlock 0x001F 0xF800 [3]]
      levels <- decoded (file 1)
      [pixelAt (level0 levels) x 0 | x <- [0, 1]] @?= [PixelRGBA8 0 0 0 0, blue]
      opaque <- decoded (file 0)
      pixelAt (level0 opaque) 0 0 @?= PixelRGBA8 0 0 0 255
  , testCase "DXT3 and DXT5 keep their alpha" $ do
      dxt3 <- decoded dxt3File
      [a | y <- [0 .. 3], x <- [0 .. 3], let PixelRGBA8 _ _ _ a = pixelAt (level0 dxt3) x y] @?= [17 * n | n <- [0 .. 15]]
      dxt5 <- decoded dxt5File
      [pixelAt (level0 dxt5) x 0 | x <- [0, 1]] @?= [PixelRGBA8 255 0 0 0, red]
  , testCase "alpha depth 0 makes DXT5 and BGRA levels opaque" $ do
      dxt5 <- decoded $ setByte 9 0 dxt5File
      pixelAt (level0 dxt5) 0 0 @?= red
      bgra <- decoded (bgraFile 0)
      map (\l -> pixelAt l 0 0) bgra @?= [PixelRGBA8 30 20 10 255, PixelRGBA8 3 2 1 255]
  , testCase "BGRA is stored blue first" $ do
      levels <- decoded (bgraFile 8)
      map size levels @?= [(2, 1), (1, 1)]
      [pixelAt (level0 levels) x 0 | x <- [0, 1]] @?= [PixelRGBA8 30 20 10 40, PixelRGBA8 70 60 50 80]
      pixelAt (levels !! 1) 0 0 @?= PixelRGBA8 3 2 1 4
  , testCase "paletted content decodes as in BLP1" $ do
      levels <- decoded (palettedFile 8)
      [pixelAt (level0 levels) x y | (x, y) <- [(0, 0), (1, 0), (0, 1), (1, 1)]]
        @?= [PixelRGBA8 0 0 255 255, PixelRGBA8 255 0 0 128, PixelRGBA8 255 0 0 64, PixelRGBA8 0 0 255 0]
      noAlpha <- decoded (palettedFile 0)
      pixelAt (level0 noAlpha) 1 0 @?= red
      oneBit <- decoded (palettedFile 1)
      [pixelAt (level0 oneBit) x y | (x, y) <- [(0, 0), (1, 0), (0, 1), (1, 1)]]
        @?= [PixelRGBA8 0 0 255 0, red, red, PixelRGBA8 0 0 255 0]
  , testCase "a damaged mipmap ends the list, a damaged level 0 fails the file" $ do
      levels <- decoded $ blp2 dxt1Header blankPalette [dxt1Mip0, BS.take 4 (colourBlock 0xFFFF 0 []), dxt1Block 0x07E0, dxt1Block 0x001F]
      map size levels @?= [(8, 8)]
      assertLeft "Mipmap 0" . decodeBlp $ blp2 dxt1Header blankPalette [BS.take 31 dxt1Mip0]
      assertLeft "BGRA" . decodeBlp $ blp2 defHeader2 { h2Encoding = 3, h2Width = 2, h2Height = 1 } blankPalette [BS.pack [1 .. 7]]
  , testCase "unsupported and malformed headers are errors" $ do
      assertLeft "JPEG" . decodeBlp $ setDword 4 0 dxt1File
      assertLeft "type" . decodeBlp $ setDword 4 5 dxt1File
      assertLeft "encoding" . decodeBlp $ setByte 8 4 dxt1File
      assertLeft "alpha type" . decodeBlp $ setByte 10 3 dxt1File
      assertLeft "Invalid image size" . decodeBlp $ setDword 12 0 dxt1File
      assertLeft "No mipmap data" . decodeBlp $ blp2 dxt1Header blankPalette []
      assertLeft "not enough input" . decodeBlp $ BS.take 100 dxt1File
      assertLeft "JPEG" . blpInfo $ setDword 4 0 dxt1File
  , testCase "the size limit applies to BLP2" $
      assertLeft "limit" $ decodeBlpWith (BlpDecodeOptions 63) dxt1File
  ]

-- | Valid BLP2 files of every content type, for the fuzz properties.
blp2Seeds :: [ByteString]
blp2Seeds = [dxt1File, dxt3File, dxt5File, bgraFile 8, palettedFile 8]

decoded :: ByteString -> IO [Image PixelRGBA8]
decoded file = case decodeBlpMipmaps file of
  Left e -> assertFailure e
  Right levels -> return (map convertRGBA8 levels)

size :: Image a -> (Int, Int)
size img = (imageWidth img, imageHeight img)

assertLeft :: String -> Either String a -> Assertion
assertLeft needle r = case r of
  Left e -> assertBool ("error " ++ show e ++ " should mention " ++ show needle) (needle `isInfixOf` e)
  Right _ -> assertFailure "expected a decoding error"

setByte :: Int -> Word8 -> ByteString -> ByteString
setByte o v bs = BS.take o bs <> BS.singleton v <> BS.drop (o + 1) bs

red, green, blue, white :: PixelRGBA8
red = PixelRGBA8 255 0 0 255
green = PixelRGBA8 0 255 0 255
blue = PixelRGBA8 0 0 255 255
white = PixelRGBA8 255 255 255 255

-- | A DXT1 block of one colour.
dxt1Block :: Int -> ByteString
dxt1Block c = colourBlock c 0 []

dxt1Header :: Header2
dxt1Header = defHeader2 { h2Width = 8, h2Height = 8 }

-- | 8x8: red, green / blue, white.
dxt1Mip0 :: ByteString
dxt1Mip0 = BS.concat (map dxt1Block [0xF800, 0x07E0, 0x001F, 0xFFFF])

dxt1File :: ByteString
dxt1File = blp2 dxt1Header blankPalette [dxt1Mip0, dxt1Block 0xFFFF, dxt1Block 0x07E0, dxt1Block 0x001F]

-- | 4x4, alpha 17 * i at pixel i.
dxt3File :: ByteString
dxt3File = blp2 defHeader2 { h2AlphaDepth = 8, h2AlphaType = 1 } blankPalette [dxt3Alpha [0 .. 15] <> dxt1Block 0xF800]

-- | 4x4 red, transparent at pixel 0.
dxt5File :: ByteString
dxt5File = blp2 defHeader2 { h2AlphaDepth = 8, h2AlphaType = 7 } blankPalette [dxt5Alpha 255 0 [1] <> dxt1Block 0xF800]

-- | 2x1 and 1x1 levels with the given alpha depth.
bgraFile :: Word8 -> ByteString
bgraFile depth = blp2 defHeader2 { h2Encoding = 3, h2AlphaDepth = depth, h2AlphaType = 8, h2Width = 2, h2Height = 1 }
  blankPalette [BS.pack [10, 20, 30, 40, 50, 60, 70, 80], BS.pack [1, 2, 3, 4]]

-- | 2x2, blue and red entries, with 8-bit alpha 255, 128, 64, 0, 1-bit
-- alpha on pixels 1 and 2, or none. The alpha type is 8, as in real files.
palettedFile :: Word8 -> ByteString
palettedFile depth = blp2 defHeader2 { h2Encoding = 1, h2AlphaDepth = depth, h2AlphaType = 8, h2Width = 2, h2Height = 2, h2HasMips = 0 }
  ([(255, 0, 0, 0), (0, 0, 255, 0)] ++ replicate 254 (0, 0, 0, 0))
  [BS.pack ([0, 1, 1, 0] ++ alphaList)]
  where
    alphaList = case depth of
      8 -> [255, 128, 64, 0]
      1 -> [0x06] -- pixels 1 and 2
      _ -> []

-- | The full size image of a decoded file.
level0 :: [Image PixelRGBA8] -> Image PixelRGBA8
level0 (l : _) = l
level0 [] = error "no levels decoded"
