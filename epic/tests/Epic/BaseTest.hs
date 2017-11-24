{-# LANGUAGE QuasiQuotes #-}

module Epic.BaseTest
  ( mkBaseTests
  ) where

import           GHC.Generics (Generic)

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.ST

import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           TestHelpers

import           Epic.Base
import           Epic.Conversion
import           Epic.Evaluation
import           Epic.Language
import           Epic.Loader
import           Epic.PrettyPrinter
import           Epic.Resolver
import           Epic.TH
import           Epic.TypeChecker

data EpicTest
  = EpicTestGroup T.Text [EpicTest]
  | EpicTest T.Text T.Text
  deriving Generic

instance FromEpic EpicTest

toTestTree :: EpicTest -> TestTree
toTestTree = \case
  EpicTestGroup txt tests -> testGroup (T.unpack txt) (map toTestTree tests)
  EpicTest title err -> testCase (T.unpack title) (assertString (T.unpack err))

evalMod :: Module
evalMod = [epic|module Eval
import Tests
import TestHelpers
tests : TestTree
tests = TestGroup "base" allTests
|]

mkBaseTests :: IO TestTree
mkBaseTests = do
  eRes <- runExceptT $ do
    modules <- fmap (map snd) $
      loadModules baseModules ["tests/base"] [ModuleName ["Tests"]]

    resolvedModules <- resolveModules $ evalMod : modules

    foreigns <- lift $ stToIO baseForeigns
    typedModules <- typeCheckModules (Just (map fst foreigns)) resolvedModules

    let mTerm = listToMaybe
          [ term | _mod <- typedModules
                 , Definition (TermDefinition name term _)
                     <- _mod ^. definitions
                 , _mod ^. moduleName == ModuleName ["Eval"]
                 , name == "tests" ]
    term <- case mTerm of
      Nothing -> throwError "can't find term `tests`"
      Just t  -> return t

    res <- lift $ stToIO $ fromEpic =<< evalWHNF foreigns term
    return $ toTestTree res

  return $ case eRes of
    Left err ->
      testCase "loads the tests for base" $ assertFailure $ T.unpack err
    Right tree -> tree
