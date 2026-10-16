module Statistics(
    collectSamples 
  , printStatistics
  , collectStatistics
  ) where

import Codec.Picture.Blp.Internal.Data
import Codec.Picture.Blp (BlpVersion(..))
import Codec.Picture.Blp.Internal.Parser
import Control.Monad
import Data.Char (toLower)
import Data.Word 
import System.Directory
import System.FilePath
import System.IO

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.List as L 

import File 

-- | What files are grouped by: version, content type and the header fields
-- that vary between files, with their names.
data BlpInfo = BlpInfo BlpVersion BlpCompression [(String, Word32)]
  deriving (Eq, Ord)

type BlpStat = (FilePath, Int)

type StatMap = M.Map BlpInfo BlpStat

collectSamples :: FilePath -> IO StatMap
collectSamples dir = do 
  f <- doesDirectoryExist dir
  unless f $ fail $ "Provided directory " ++ dir ++ " doesn't exists!"
  forEachFile True dir ((".blp" ==) . map toLower . takeExtension) collectSample M.empty

collectSample :: StatMap -> FilePath -> IO StatMap
collectSample stats inputFile = do
  putStrLn $ "Sampling " ++ inputFile
  result <- tryAll (parseBlpFile <$> BS.readFile inputFile)
  case join result of
    Left err -> do
      -- One unreadable file should not lose the statistics of the rest.
      hPutStrLn stderr $ inputFile ++ ": skipped, " ++ err
      return stats
    Right blp -> return $ adjust' (updateStat inputFile) (blpType blp) stats

blpType :: BlpFile -> BlpInfo
blpType (BlpFile1 blp@BlpStruct{..}) = BlpInfo Blp1 (blpCompression blp)
  [("alpha", blpAlphaBits), ("extra", blpExtra), ("mips", blpHasMipmaps)]
blpType (BlpFile2 blp@Blp2Struct{..}) = BlpInfo Blp2 (blp2Compression blp)
  [("alpha", fromIntegral blp2AlphaDepth), ("alphaType", fromIntegral blp2AlphaType), ("mips", fromIntegral blp2HasMips)]

adjust' :: (Maybe BlpStat -> BlpStat) -> BlpInfo -> StatMap -> StatMap 
adjust' f k m = M.insert k (f $ M.lookup k m) m

updateStat :: FilePath -> Maybe BlpStat -> BlpStat 
updateStat path Nothing = (path, 1)
updateStat _ (Just (p, i)) = (p, i+1)

printStatistics :: FilePath -> StatMap -> IO () 
printStatistics dir stats = do
    createDirectoryIfMissing True dir
    forM_ (M.toList stats) $ \(BlpInfo version compression fields, (path, i)) -> do
      let postfix = L.intercalate "-" $ [show version | version /= Blp1] ++ show compression : [name ++ show v | (name, v) <- fields]
      copyFile path $ dir </> dropExtension (takeFileName path) ++ "-" ++ postfix ++ ".blp"
      putStrLn $ postfix ++ ": " ++ show i ++ " images"

collectStatistics :: FilePath -> FilePath -> IO ()
collectStatistics dir outputDir = printStatistics outputDir =<< collectSamples dir 
