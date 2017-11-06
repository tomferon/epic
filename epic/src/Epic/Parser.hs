module Epic.Parser
  ( parseTerm
  , parseModule
  , moduleNameParser
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except

import           Data.Attoparsec.Text as AP
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T

import           Epic.Language
import           Epic.Parser.Internal

parseModule :: MonadError T.Text m => T.Text -> m Module
parseModule =
  either (throwError . T.pack) return . parseOnly (moduleParser <* endOfInput)

parseTerm :: T.Text -> Either T.Text LocalTerm
parseTerm =
  either (Left . T.pack) Right
    . parseOnly (termParser "" operators True True True True [] <* endOfInput)
