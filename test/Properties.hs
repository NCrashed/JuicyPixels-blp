-- | Property tests: the decoder on damaged files, and encode/decode round
-- trips on random images.
module Properties(propertyTests) where

import Codec.Picture
import Codec.Picture.Blp
import Control.DeepSeq (force)
import Control.Exception (SomeAsyncException, SomeException, evaluate, fromException, throwIO, try)
import Data.Bits
import Data.ByteString (ByteString)
import Data.Word
import System.Timeout (timeout)
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, monitor, run)
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Blp2 (blp2Seeds)

propertyTests :: TestTree
propertyTests = testGroup "properties"
  [ testGroup "fuzz"
    [ testProperty "decoding a damaged file never throws" decodingNeverThrows
    , testProperty "blpInfo on a damaged file never throws" infoNeverThrows
    ]
  , testProperty "every level has its expected size" levelSizes
  , testProperty "images with few colours survive the palette exactly" fewColoursExact
  ]

-- | Valid files of every kind, to be damaged.
seeds :: [ByteString]
seeds = blp2Seeds ++
  [ encode (BlpFormatJpeg 90) 16 (texture 16 8)
  , encode (BlpFormatJpeg 50) 1 (texture 5 7)
  , encode (BlpFormatJpeg 90) 16 (opaque 8 8)
  , encode (BlpFormatPaletted True) 16 (texture 16 16)
  , encode (BlpFormatPaletted False) 16 (texture 9 4)
  ]
  where
    opaque w h = ImageRGBA8 . pixelMap (\(PixelRGBA8 r g b _) -> PixelRGBA8 r g b 255) . convertRGBA8 $ texture w h

texture :: Int -> Int -> DynamicImage
texture w h = ImageRGBA8 $ generateImage px w h
  where
    px x y = PixelRGBA8 (fromIntegral $ x * 37) (fromIntegral $ y * 59) (fromIntegral $ x * y) (fromIntegral $ 255 - x * 13)

encode :: BlpFormat -> Int -> DynamicImage -> ByteString
encode format levels = BSL.toStrict . either (error . show) id . encodeBlpWith defaultBlpEncodeOptions { blpFormat = format, blpMipLevels = levels }

-- | One way of damaging a file.
data Damage
  = Flip Int Word8 -- ^ XOR a byte
  | Truncate Int -- ^ Keep only a prefix
  | SetDword Int Word32 -- ^ Overwrite a header dword
  deriving Show

damage :: ByteString -> Damage -> ByteString
damage bs d = case d of
  Flip i v
    | i < BS.length bs -> BS.take i bs <> BS.singleton (BS.index bs i `xor` v) <> BS.drop (i + 1) bs
    | otherwise -> bs
  Truncate n -> BS.take n bs
  SetDword o v
    | o + 4 <= BS.length bs -> BS.take o bs <> BS.pack [fromIntegral (v `shiftR` (8 * k)) | k <- [0 .. 3]] <> BS.drop (o + 4) bs
    | otherwise -> bs

-- | A seed file and a few kinds of damage to apply to it.
damagedFile :: Gen (ByteString, [Damage])
damagedFile = do
  seed <- elements seeds
  let len = BS.length seed
      flipAny = Flip <$> choose (0, len - 1) <*> choose (1, 255)
      truncate' = Truncate <$> choose (0, len)
      -- Header fields and mipmap slots, set to small, boundary and huge values.
      header = SetDword
        <$> elements [4, 8 .. 152]
        <*> oneof [choose (0, 2000), elements [0xFF, 0xFFFF, 0x10000, 0x7FFFFFFF, 0xFFFFFFFF]]
  ds <- resize 4 . listOf1 $ frequency [(5, flipAny), (1, truncate'), (3, header)]
  return (seed, ds)

-- | Decode fully. JuicyPixels loops forever on some corrupt JPEG data
-- (haskell-github-trust/Juicy.Pixels#230); such cases are counted, not
-- failed. Anything thrown is a failure.
decodingNeverThrows :: Property
decodingNeverThrows = forAllShow damagedFile showCase $ \(seed, ds) -> monadicIO $ do
  let file = foldl damage seed ds
  result <- run . timeout 1000000 . trySync . evaluate . force $
    fmap (map (imageData . convertRGBA8)) (decodeBlpMipmaps file)
  case result of
    Nothing -> monitor (label "JuicyPixels hang (Juicy.Pixels#230)")
    Just (Left e) -> do
      monitor (counterexample $ "threw: " ++ show (e :: SomeException))
      assert False
    Just (Right decoded) -> monitor (label $ either (const "Left") (const "Right") decoded)

infoNeverThrows :: Property
infoNeverThrows = forAllShow damagedFile showCase $ \(seed, ds) -> monadicIO $ do
  result <- run . trySync . evaluate . force $ either Left (Right . show) (blpInfo (foldl damage seed ds))
  case result of
    Left e -> monitor (counterexample $ "threw: " ++ show (e :: SomeException)) >> assert False
    Right _ -> return ()

-- | Like 'try', but lets asynchronous exceptions through, so that 'timeout'
-- still sees its own.
trySync :: IO a -> IO (Either SomeException a)
trySync action = try action >>= either rethrowAsync (return . Right)
  where
    rethrowAsync e = case fromException e of
      Just async -> throwIO (async :: SomeAsyncException)
      Nothing -> return (Left e)

showCase :: (ByteString, [Damage]) -> String
showCase (seed, ds) = "seed of " ++ show (BS.length seed) ++ " bytes, damage " ++ show ds

-- | Level l of a w x h texture is max 1 (w >> l) by max 1 (h >> l), down to
-- 1x1, at most 16 levels.
levelSizes :: Property
levelSizes = forAll ((,,) <$> choose (1, 40) <*> choose (1, 40) <*> format) $ \(w, h, f) ->
  let expected = take 16 (chain w h)
      decoded = decodeBlpMipmaps (encode f 16 (texture w h))
  in either (const []) (map (\i -> (dynamicMap imageWidth i, dynamicMap imageHeight i))) decoded === expected
  where
    format = elements [BlpFormatJpeg 90, BlpFormatPaletted True, BlpFormatPaletted False]
    chain w h = go 0
      where
        go l = let s = (max 1 (w `shiftR` l), max 1 (h `shiftR` l))
               in s : if s == (1, 1) then [] else go (l + 1)

-- | An opaque image with at most 16 colours fits the palette, so mip 0 comes
-- back exactly.
fewColoursExact :: Property
fewColoursExact = forAll gen $ \(w, h, colours, picks) ->
  let img = generateImage (\x y -> colours !! (picks !! (y * w + x))) w h
      decoded = decodeBlp (encode (BlpFormatPaletted False) 1 (ImageRGBA8 img))
  in fmap (imageData . convertRGBA8) decoded === Right (imageData img)
  where
    gen = do
      w <- choose (1, 24)
      h <- choose (1, 24)
      n <- choose (1, 16)
      colours <- vectorOf n (PixelRGBA8 <$> arbitrary <*> arbitrary <*> arbitrary <*> pure 255)
      picks <- vectorOf (w * h) (choose (0, n - 1))
      return (w, h, colours, picks)
