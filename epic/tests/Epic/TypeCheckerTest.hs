module Epic.TypeCheckerTest where

import Epic.Language
import Epic.TypeChecker.Internal

import TestHelpers

typeCheckerTests :: TestTree
typeCheckerTests = testGroup "Epic.TypeChecker"
  [ toMetaTests, substMetaTests ]

toMetaTests :: TestTree
toMetaTests = testGroup "toMeta"
  [ testCase "transforms a Type into a MetaType" $ do
      toMeta (FunctionType PrimTypeInt PrimTypeBool)
        @?= (FunctionTypeM PrimTypeIntM PrimTypeBoolM :: MetaType)

  , testCase "transforms a Kind into a MetaKind" $ do
      toMeta (Arrow Star Star) @?= (ArrowM StarM StarM :: MetaKind)
  ]

substMetaTests :: TestTree
substMetaTests = testGroup "substMeta"
  [
  ]
