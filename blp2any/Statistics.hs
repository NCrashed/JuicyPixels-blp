module Statistics(
    collectSamples 
  , printStatistics
  , collectStatistics
  ) where

import Codec.Picture.Blp.Internal.Data
import Codec.Picture.Blp.Internal.Parser
import Control.Monad 
import Data.Word 
import System.Directory
import System.FilePath

import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as H 
import qualified Data.List as L 

import File 

type BlpInfo = (BlpCompression, [BlpFlag], BlpPictureType, Word32)
type BlpStat = (FilePath, Int)

type StatMap = H.HashMap BlpInfo BlpStat

collectSamples :: FilePath -> IO StatMap
collectSamples dir = do 
  f <- doesDirectoryExist dir
  unless f $ fail $ "Provided directory " ++ dir ++ " doesn't exists!"
  forEachFile dir ((".blp" ==).takeExtension) collectSample H.empty
  
collectSample :: StatMap -> FilePath -> IO StatMap 
collectSample stats inputFile = do 
  putStrLn $ "Sampling " ++ inputFile
  fc <- BS.readFile inputFile
  case parseBlp fc of 
    Left err -> fail $ inputFile ++ ": " ++ err 
    Right blp -> return $ adjust' (updateStat inputFile) (blpType blp) stats

blpType :: BlpStruct -> BlpInfo 
blpType BlpStruct{..} = (blpCompression, blpFlags, blpPictureType, blpPictureSubType)

adjust' :: (Maybe BlpStat -> BlpStat) -> BlpInfo -> StatMap -> StatMap 
adjust' f k m = H.insert k (f $ H.lookup k m) m

updateStat :: FilePath -> Maybe BlpStat -> BlpStat 
updateStat path Nothing = (path, 1)
updateStat _ (Just (p, i)) = (p, i+1)

printStatistics :: FilePath -> StatMap -> IO () 
printStatistics dir stats = do
    createDirectoryIfMissing True dir
    forM_ (H.toList stats) $ \((compression, flags, pictureType, pictureSubType), (path, i)) -> do 
      let postfix = L.intercalate "-" [show compression, show flags, show pictureType, show pictureSubType]
      copyFile path $ dir ++ "/" ++ dropExtension (takeFileName path) ++ "-"
        ++ postfix ++ ".blp"
      putStrLn $ postfix ++ ": " ++ show i ++ " images"

collectStatistics :: FilePath -> FilePath -> IO ()
collectStatistics dir outputDir = printStatistics outputDir =<< collectSamples dir 
