module Epic.TypeChecker
  ( typeOfModule
  , typeOfModules
  ) where

import Debug.Trace

import           Control.Applicative
import           Control.Lens hiding (Context)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

import           Data.Char (isLower)
import           Data.Functor.Foldable
import           Data.List
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T

import           Epic.Language
import           Epic.PrettyPrinter
import           Epic.Resolver
import           Epic.TypeChecker.Internal

typeOfModule :: Maybe [T.Text] -> FQModule -> TypeChecker TypedModule
typeOfModule mForeignNames _mod = do
    _mod' <- forOf (types . each) _mod
                   (kindCheckTypeDefinition (_mod ^. moduleName))
    forOf (definitions . each) _mod' (typeCheckDefinition (_mod ^. moduleName))

typeOfModules :: MonadError T.Text m => Maybe [T.Text] -> [FQModule]
              -> m [TypedModule]
typeOfModules mForeignNames mods = either throwError return $
  evalStateT (reorderModules mods >>= mapM (typeOfModule mForeignNames))
             emptyTypeCheckerState
