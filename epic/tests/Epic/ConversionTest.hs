module Epic.ConversionTest
  ( conversionTests
  ) where

import GHC.Generics

import Control.Monad
import Control.Monad.ST

import Epic.Conversion
import Epic.Evaluation
import Epic.Language

import TestHelpers

conversionTests :: TestTree
conversionTests = testGroup "Epic.Conversion"
  [ gtoEpicTests, gfromEpicTests ]

data DummyType
  = FirstConstructor Bool
  | SecondConstructor Int Bool
  deriving (Eq, Show, Generic)

gtoEpicTests :: TestTree
gtoEpicTests = testGroup "GToEpic"
  [ testCase "returns (0, []) for ()" $
      join $ stToIO $ do
        (n, ts) <- gtoEpic $ from ()
        return $ do
          n @?= 0
          assertBool "should be empty" $ null ts

  , testCase "returns (0, [thunk(2), thunk(1), thunk(0)]) for (0, 1, 2)" $ do
      join $ stToIO $ do
        (n, ts) <- gtoEpic $ from (0 :: Int, 1 :: Int, 2 :: Int)
        return $ case ts of
          [BaseTerm t2, BaseTerm t1, BaseTerm t0] -> do
            n @?= 0
            t2 @?= PrimInt 2
            t1 @?= PrimInt 1
            t0 @?= PrimInt 0
          _ -> assertFailure "should be [BaseTerm _, BaseTerm _, BaseTerm _]"

  , testCase "returns (0, []) for False and (1, []) for True" $
      join $ stToIO $ do
        (n,  ts)  <- gtoEpic $ from False
        (n', ts') <- gtoEpic $ from True
        return $ do
          n  @?= 0
          n' @?= 1
          assertBool "should be empty" $ null ts
          assertBool "should be empty" $ null ts'

  , testCase "returns (1, [thunk(True), thunk(42)]) for SecondConstructor 42\
             \ True" $ do
      join $ stToIO $ do
        (n, ts)  <- gtoEpic $ from $ SecondConstructor 42 True
        return $ case ts of
          [BaseTerm t, BaseTerm t'] -> do
            n  @?= 1
            t  @?= PrimBool True
            t' @?= PrimInt 42
          _ -> assertFailure "should be [BaseTerm _, BaseTerm _]"
  ]

gfromEpicTests :: TestTree
gfromEpicTests = testGroup "GFromEpic"
  [ testCase "returns () for Constructor 0 []" $
      runST (to <$> gfromEpic (0, [])) @?= ()

  , testCase "returns (0, 1, 2) for (0, [BaseTerm (PrimInt 0), BaseTerm\
             \ (PrimInt 1), BaseTerm (PrimInt 2)])" $ do
      let ets = [ BaseTerm (PrimInt 0)
                , BaseTerm (PrimInt 1)
                , BaseTerm (PrimInt 2) ]
      runST (to <$> gfromEpic (0, ets)) @?= (0 :: Int, 1 :: Int, 2 :: Int)

  , testCase "returns False for (0, []) and True for (1, [])" $ do
      runST (to <$> gfromEpic (0, [])) @?= False
      runST (to <$> gfromEpic (1, [])) @?= True

  , testCase "returns SecondConstructor 42 True for (1, [BaseTerm (PrimInt 42),\
             \ BaseTerm (PrimBool True)])" $
      runST (to <$> gfromEpic (1, [ BaseTerm (PrimInt 42)
                                  , BaseTerm (PrimBool True) ]))
        @?= SecondConstructor 42 True
  ]
