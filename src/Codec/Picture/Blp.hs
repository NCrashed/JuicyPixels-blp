module Codec.Picture.Blp(
    decodeBlp
  ) where

import Codec.Picture
import Control.Monad
import Data.ByteString (ByteString)
import Data.Monoid

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
    BlpUncompressed1 {..} -> fail "unimplemented BlpUncompressed1"
    BlpUncompressed2 {..} -> fail "unimplemented BlpUncompressed2"