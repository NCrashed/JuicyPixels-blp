{-# OPTIONS_GHC -Wno-deprecations #-}
-- | Tests for the encoders that 'encodeBlpWith' replaced. They live in their
-- own module so that deprecation warnings stay errors everywhere else.
module Deprecated(deprecatedTests) where

import Codec.Picture
import Codec.Picture.Blp
import Control.Exception (ErrorCall, evaluate, try)
import Data.Either (isLeft)
import Data.Int (Int64)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString.Lazy as BSL

deprecatedTests :: TestTree
deprecatedTests = testGroup "deprecated encoders"
  [ testCase "match encodeBlpWith" $ do
      let with f n = either (error . show) id $ encodeBlpWith (BlpEncodeOptions f n) img
      encodeBlpJpeg 90 3 img @?= with (BlpFormatJpeg 90) 3
      encodeBlpUncompressedWithAlpha 3 img @?= with (BlpFormatPaletted True) 3
      encodeBlpUncompressedWithoutAlpha 3 img @?= with (BlpFormatPaletted False) 3
  , testCase "clamp the JPEG quality" $ do
      encodeBlpJpeg 300 1 img @?= encodeBlpJpeg 100 1 img
      encodeBlpJpeg (-5) 1 img @?= encodeBlpJpeg 1 1 img
  , testCase "throw on an empty image" $ do
      r <- try (evaluate . BSL.length $ encodeBlpJpeg 90 1 (ImageRGBA8 $ generateImage (\_ _ -> PixelRGBA8 0 0 0 0) 0 3))
      assertBool "expected an error" (isLeft (r :: Either ErrorCall Int64))
  ]
  where
    img = ImageRGBA8 $ generateImage (\x y -> PixelRGBA8 (fromIntegral $ x * 16) (fromIntegral $ y * 16) 128 200) 16 16
