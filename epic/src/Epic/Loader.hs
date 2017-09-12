module Epic.Loader where

import           Control.Lens ((^.))
import           Control.Monad.Except

import           Data.Monoid
import qualified Data.Text    as T
import qualified Data.Text.IO as T

import           System.Directory (doesFileExist, makeAbsolute)
import           System.FilePath ((</>), (<.>))

import           Epic.Language hiding (name)
import           Epic.Parser

loadModules :: (MonadIO m, MonadError T.Text m) => [FilePath] -> [ModuleName]
            -> m [((FilePath, FilePath), Module)]
loadModules dirs = go []
  where
    go :: (MonadIO m, MonadError T.Text m) => [((FilePath, FilePath), Module)]
       -> [ModuleName] -> m [((FilePath, FilePath), Module)]
    go acc [] = return acc
    go acc (name : names)
      | name `elem` map (_moduleName . snd) acc = go acc names
      | otherwise = do
          res@(_, _mod) <- loadModule dirs name
          go (res : acc) (names ++ _mod ^. imports)

loadModule :: (MonadIO m, MonadError T.Text m) => [FilePath] -> ModuleName
           -> m ((FilePath, FilePath), Module)
loadModule [] name =
  throwError $ "can't find module " <> T.intercalate "." name
loadModule (dir : dirs) name = do
  let relpath = foldr (</>) "" (map T.unpack name) <.> "epic"
      path = dir </> relpath
  check <- liftIO $ doesFileExist path
  if check
    then do
      contents <- liftIO $ T.readFile path
      _mod <- parseModule contents
      return ((dir, relpath), _mod)
    else loadModule dirs name
