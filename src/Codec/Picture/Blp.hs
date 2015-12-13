module Codec.Picture.Blp(
    decodeBlp
  ) where

import Codec.Picture
import Control.Monad
import Data.ByteString (ByteString)
import Data.Monoid
import Data.Word 

import qualified Data.ByteString as BS
import qualified Data.Vector as V 

import Codec.Picture.Blp.Internal.Data
import Codec.Picture.Blp.Internal.Parser 

decodeBlp :: ByteString -> Either String DynamicImage
decodeBlp bs = do 
  blp <- parseBlp bs 
  case blpExt blp of
    BlpJpeg {..} -> do 
      when (null blpJpegData) $ fail "No jpeg data"
      let jpeg = blpJpegHeader <> head blpJpegData
      decodeJpeg jpeg

    BlpUncompressed1 {..} -> do
      when (null blpU1MipMaps) $ fail "No uncompressed1 data"
      return $ ImageRGBA8 $ generateImage gen (fromIntegral $ blpWidth blp) (fromIntegral $ blpHeight blp)
      where 
      palette :: Word8 -> PixelRGBA8
      palette i = blpU1Palette V.! fromIntegral i

      makeIndex x y = y*(fromIntegral $ blpWidth blp)+x
      takeColor x y = palette $ fst (head blpU1MipMaps) `BS.index` makeIndex x y
      takeAlpha x y = snd (head blpU1MipMaps) `BS.index` makeIndex x y
      gen x y = let 
        PixelRGBA8 r g b _ = takeColor x y 
        a = takeAlpha x y 
        in PixelRGBA8 b g r a

    BlpUncompressed2 {..} -> do
      when (null blpU2MipMaps) $ fail "No uncompressed2 data"
      return $ ImageRGBA8 $ generateImage gen (fromIntegral $ blpWidth blp) (fromIntegral $ blpHeight blp)
      where 
      palette :: Word8 -> PixelRGBA8
      palette i = blpU2Palette V.! fromIntegral i

      makeIndex x y = y*(fromIntegral $ blpWidth blp)+x
      takeColor x y = palette $ head blpU2MipMaps `BS.index` makeIndex x y
      gen x y = let 
        PixelRGBA8 r g b a = takeColor x y
        in PixelRGBA8 b g r (255 - a)