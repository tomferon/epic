module Epic.TypeChecker
  ( typeCheckModule
  , typeCheckModules
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

typeCheckModule :: Maybe [T.Text] -> FQModule -> TypeChecker TypedModule
typeCheckModule mForeignNames _mod = do
    _mod' <- forOf (types . each) _mod
                   (kindCheckTypeDefinition (_mod ^. moduleName))
    forOf (definitions . each) _mod' (typeCheckDefinition (_mod ^. moduleName))

typeCheckModules :: MonadError T.Text m => Maybe [T.Text] -> [FQModule]
              -> m [TypedModule]
typeCheckModules mForeignNames mods = either throwError return $
  evalStateT (reorderModules mods >>= mapM (typeCheckModule mForeignNames))
             emptyTypeCheckerState
