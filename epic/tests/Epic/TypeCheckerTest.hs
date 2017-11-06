module Epic.TypeCheckerTest where

import Epic.Language
import Epic.TypeChecker

import TestHelpers

--prop_typeShiftXY :: Int -> Int -> Type -> Bool
--prop_typeShiftXY x y typ =
--  typeShift x (typeShift y typ) == typeShift (x + y) typ

--spec_typeOf :: Spec
--spec_typeOf = do
--  it "returns the type of a variable" $ do
--    1 + 1 `shouldBe` 2
