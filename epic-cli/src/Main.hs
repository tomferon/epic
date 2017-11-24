import           Control.Monad.Except

import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO

import qualified Codec.Archive.Tar as Tar

import           Epic.Base
import           Epic.Loader
import           Epic.Options
import           Epic.PrettyPrinter
import           Epic.Resolver
import           Epic.TypeChecker

main :: IO ()
main = do
  opts@Options{..} <- parseOptions

  eRes <- runExceptT $ do
    (paths, modules) <- fmap unzip $
      loadModules baseModules (optsBase : optsPaths) optsModules
    resolvedModules <- resolveModules modules
    typedModules <- typeCheckModules Nothing resolvedModules
    liftIO $ mapM_ (T.putStrLn . prettyPrint) typedModules
    return $ map snd $ filter ((==optsBase) . fst) $ catMaybes paths

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
