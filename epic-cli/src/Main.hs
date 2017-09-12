import           Control.Monad
import           Control.Monad.Except

import           Data.List
import qualified Data.Text as T

import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO

import qualified Codec.Archive.Tar as Tar

import           Epic.Loader
import           Epic.Options
import           Epic.TypeChecker

main :: IO ()
main = do
  opts@Options{..} <- parseOptions
  absBase <- makeAbsolute optsBase

  eRes <- runExceptT $ do
    (paths, modules) <- unzip <$> loadModules (optsBase : optsPaths) optsModules
    typeCheckModules Nothing modules
    return $ map snd $ filter ((==optsBase) . fst) paths

  case eRes of
    Left err -> do
      hPutStrLn stderr $ "Can't package: " ++ T.unpack err
      exitFailure
    Right paths -> do
      path <- mkOutputFile opts
      Tar.create path optsBase paths
      putStrLn $ "Package " ++ path ++ " created."

mkOutputFile :: Options -> IO FilePath
mkOutputFile opts = case optsOutput opts of
  Just path -> return path
  Nothing -> do
    path <- makeAbsolute $ optsBase opts
    return $ takeFileName path <.> "tar"
