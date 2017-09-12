module Epic.Options
  ( Options(..)
  , parseOptions
  ) where

import           Data.Monoid
import           Data.Attoparsec.Text (parseOnly)
import qualified Data.Text as T

import           Options.Applicative

import           Epic.Language
import           Epic.Parser (moduleNameParser)

data Options = Options
  { optsPaths   :: [FilePath]
  , optsPackage :: Bool
  , optsOutput  :: Maybe FilePath
  , optsBase    :: FilePath
  , optsModules :: [ModuleName]
  } deriving Show

parseOptions :: IO Options
parseOptions = execParser $
    info (helper <*> parser)
         (fullDesc <> progDesc "Typecheck and package an Epic application into\
                               \ a .tar file")
  where
    parser :: Parser Options
    parser = Options
      <$> many (strOption (short 'p' <> long "path" <> metavar "PATH"
                           <> help "Directories containing external modules"))
      <*> flag True False (long "no-package" <> help "Only typecheck")
      <*> optional (strOption (short 'o' <> long "output" <> metavar "FILENAME"
                               <> help "Path to of the .tar file"))
      <*> strOption (short 'b' <> long "base" <> metavar "PATH" <> value "."
                     <> help "Base path of internal modules")
      <*> some (argument moduleName (metavar "MODULES"))

    moduleName :: ReadM ModuleName
    moduleName = eitherReader (parseOnly moduleNameParser . T.pack)
