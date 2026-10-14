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
import qualified Data.Map.Strict as M
import qualified Data.List as L 

import File 

type BlpInfo = (BlpCompression, Word32, Word32, Word32)
type BlpStat = (FilePath, Int)

type StatMap = M.Map BlpInfo BlpStat

collectSamples :: FilePath -> IO StatMap
collectSamples dir = do 
  f <- doesDirectoryExist dir
  unless f $ fail $ "Provided directory " ++ dir ++ " doesn't exists!"
  forEachFile dir ((".blp" ==).takeExtension) collectSample M.empty
  
collectSample :: StatMap -> FilePath -> IO StatMap 
collectSample stats inputFile = do 
  putStrLn $ "Sampling " ++ inputFile
  fc <- BS.readFile inputFile
  case parseBlp fc of 
    Left err -> fail $ inputFile ++ ": " ++ err 
    Right blp -> return $ adjust' (updateStat inputFile) (blpType blp) stats

blpType :: BlpStruct -> BlpInfo 
blpType BlpStruct{..} = (blpCompression, blpAlphaBits, blpExtra, blpHasMipmaps)

adjust' :: (Maybe BlpStat -> BlpStat) -> BlpInfo -> StatMap -> StatMap 
adjust' f k m = M.insert k (f $ M.lookup k m) m

updateStat :: FilePath -> Maybe BlpStat -> BlpStat 
updateStat path Nothing = (path, 1)
updateStat _ (Just (p, i)) = (p, i+1)

printStatistics :: FilePath -> StatMap -> IO () 
printStatistics dir stats = do
    createDirectoryIfMissing True dir
    forM_ (M.toList stats) $ \((compression, alphaBits, extra, hasMipmaps), (path, i)) -> do 
      let postfix = L.intercalate "-" [show compression, "alpha" ++ show alphaBits, "extra" ++ show extra, "mips" ++ show hasMipmaps]
      copyFile path $ dir ++ "/" ++ dropExtension (takeFileName path) ++ "-"
        ++ postfix ++ ".blp"
      putStrLn $ postfix ++ ": " ++ show i ++ " images"

collectStatistics :: FilePath -> FilePath -> IO ()
collectStatistics dir outputDir = printStatistics outputDir =<< collectSamples dir 
