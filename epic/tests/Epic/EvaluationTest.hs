module Epic.EvaluationTest
  ( evaluationTests
  ) where

import Control.Monad
import Control.Monad.ST

import Data.STRef

import Epic.Evaluation
import Epic.Evaluation.Internal
import Epic.Language

import TestHelpers

evaluationTests :: TestTree
evaluationTests = testGroup "Epic.Evaluation"
  [ evalWHNFTests, matchPatternsTests ]

evalWHNFTests :: TestTree
evalWHNFTests = testGroup "evalWHNF"
  [ testCase "evaluates an application on an abstraction" $ do
      let term = Application (Abstraction Nothing (Variable 0)) (PrimInt 42)
      join $ stToIO $ do
        res <- evalWHNF [] term
        return $ case res of
          BaseTerm t -> t @?= PrimInt 42
          _ -> assertFailure "should be BaseTerm"

  , testCase "evaluates a reference to the value it's pointing to" $ do
      let idTerm = Abstraction Nothing (Variable 0)
          idType = Type (UniversalType
                         (FunctionType (TypeVariable 0) (TypeVariable 0)))
          idDef = Definition (TermDefinition "id" idTerm idType)
          idRef = Ref (ModuleName ["A"]) "id" idDef

          term = Application (Reference idRef) (PrimInt 42)

      join $ stToIO $ do
        res <- evalWHNF [] term
        return $ case res of
          BaseTerm t -> t @?= PrimInt 42
          _ -> assertFailure "should be BaseTerm"

  , testCase "evaluates a foreign reference to the corresponding Haskell\
              \ function" $ do
      let inc thunk = do
            BaseTerm (PrimInt x) <- evalThunk thunk
            return $ BaseTerm (PrimInt (x+1))

          incType = Type (FunctionType PrimTypeInt PrimTypeInt)
          incDef  = Definition (ForeignDefinition "inc" incType)
          incRef  = Ref (ModuleName ["A"]) "inc" incDef

          term = Application (Reference incRef) (PrimInt 42)

      join $ stToIO $ do
        res <- evalWHNF [("inc", HaskellFunction inc)] term
        return $ case res of
          BaseTerm t -> t @?= PrimInt 43
          _ -> assertFailure "should be BaseTerm"

  , testCase "evaluates an application to a constructor" $ do
      let typeDef = TypeDefinition "SomeType" []
                                   [("SomeConstructor", [Type PrimTypeInt])]
          typeRef = Ref (ModuleName ["A"]) "SomeType" typeDef

          term = Application (ConstructorReference typeRef "SomeConstructor")
                             (PrimInt 100)

      join $ stToIO $ do
        res <- evalWHNF [] term
        case res of
          Constructor 0 [thunk] -> do
            et <- evalThunk thunk
            case et of
              BaseTerm t -> return $ t @?= PrimInt 100
              _ -> return $ assertFailure "should be BaseTerm"
          _ -> return $ assertFailure "should be Constructor 0 [_]"

  , testCase "evaluates the correct branch of an if-then-else" $ do
      let term b = IfThenElse (PrimBool b) (PrimInt 1) (PrimInt 2)

      join $ stToIO $ do
        res <- evalWHNF [] (term True)
        return $ case res of
          BaseTerm t -> t @?= PrimInt 1
          _ -> assertFailure "should be BaseTerm"

      join $ stToIO $ do
        res <- evalWHNF [] (term False)
        return $ case res of
          BaseTerm t -> t @?= PrimInt 2
          _ -> assertFailure "should be BaseTerm"

  , testCase "evaluates an application to fix recursively" $ do
      -- fix (\f -> \b -> if b then 0 else f true)
      let func = Application FixTerm
                 (Abstraction Nothing
                  (Abstraction Nothing
                   (IfThenElse (Variable 0)
                    (PrimInt 0)
                    (Application (Variable 1) (PrimBool True)))))
          term = Application func (PrimBool False)

      join $ stToIO $ do
        res <- evalWHNF [] term
        return $ case res of
          BaseTerm t -> t @?= PrimInt 0
          _ -> assertFailure "should be BaseTerm"

  , testCase "matches patterns" $ do
      let list = Application
                 (Application
                  (ConstructorReference listTypeRef "Cons")
                  (PrimInt 1337))
                 (ConstructorReference listTypeRef "Nil")
          term = PatternMatch list
            [ (ConstructorPattern listTypeRef "Cons"
               [ VariablePattern "x"
               , ConstructorPattern listTypeRef "Nil" [] ], Variable 0)
            , (VariablePattern "x", Variable 0) ]

      join $ stToIO $ do
        res <- evalWHNF [] term
        return $ case res of
          BaseTerm t -> t @?= PrimInt 1337
          _ -> assertFailure "should be BaseTerm"

  , testCase "has a non-strict evaluation" $ do
      join $ stToIO $ do
        calledRef <- newSTRef False

        let inc thunk = do
              writeSTRef calledRef True
              BaseTerm (PrimInt x) <- evalThunk thunk
              return $ BaseTerm (PrimInt (x+1))

            incType = Type (FunctionType PrimTypeInt PrimTypeInt)
            incDef  = Definition (ForeignDefinition "inc" incType)
            incRef  = Ref (ModuleName ["A"]) "inc" incDef

            term = Application
                   (Application
                    (ConstructorReference listTypeRef "Cons")
                    (Application (Reference incRef) (PrimInt 42)))
                   (ConstructorReference listTypeRef "Nil")

        res <- evalWHNF [("inc", HaskellFunction inc)] term
        case res of
          Constructor 0 [_, thunk] -> do
            called  <- readSTRef calledRef
            et      <- evalThunk thunk
            called' <- readSTRef calledRef

            return $ case et of
              BaseTerm t -> do
                called  @?= False
                t       @?= PrimInt 43
                called' @?= True
              _ -> assertFailure "should be BaseTerm"
          _ -> return $ assertFailure "should be Constructor 0 [_, _]"
  ]

matchPatternsTests :: TestTree
matchPatternsTests = testGroup "matchPatterns"
  [ testCase "returns the first branch that the term matches" $ do
      join $ stToIO $ do
        thunk <- makeThunk $ do
          thunk0 <- makeThunk $ return $ BaseTerm $ PrimBool True
          thunk1 <- makeThunk $ return $ Constructor 1 []
          return $ Constructor 0 [thunk1, thunk0]

        (t, thunks) <- matchPatterns thunk
          [ (ConstructorPattern listTypeRef "Nil" [], PrimInt 1)
          , (WildcardPattern, PrimInt 2) ]

        return $ case thunks of
          [] -> t @?= PrimInt 2
          _ -> assertFailure "extra context should be empty"

  , testCase "returns the values to assign to the variables" $ do
      join $ stToIO $ do
        thunk <- makeThunk $ do
          thunk0 <- makeThunk $ return $ BaseTerm $ PrimInt 1
          thunk1 <- makeThunk $ do
            thunk2 <- makeThunk $ return $ BaseTerm $ PrimInt 2
            thunk3 <- makeThunk $ return $ Constructor 1 []
            return $ Constructor 0 [thunk3, thunk2]
          return $ Constructor 0 [thunk1, thunk0]

        (t, thunks) <- matchPatterns thunk
          [ (ConstructorPattern listTypeRef "Nil" [], PrimBool False)
          , (ConstructorPattern listTypeRef "Cons"
             [ VariablePattern "x"
             , ConstructorPattern listTypeRef "Cons"
                 [ VariablePattern "y"
                 , VariablePattern "z" ]], PrimBool True) ]

        ctx <- mapM evalThunk thunks
        return $ case ctx of
          [Constructor 1 [], BaseTerm y, BaseTerm x] -> do
            t @?= PrimBool True
            x @?= PrimInt 1
            y @?= PrimInt 2
          _ -> assertFailure
            "extra context should be [Constructor 1 [], BaseTerm _, BaseTerm _]"
  ]

listTypeRef :: Ref (TypeDefinition Type Kind)
listTypeRef = Ref (ModuleName ["A"]) "List" (mkTypeDef listTypeRef)
  where
    mkTypeDef :: Ref (TypeDefinition Type Kind) -> TypeDefinition Type Kind
    mkTypeDef ref = TypeDefinition "List" [Star]
      [ ("Cons", [ Type (TypeVariable 0)
                 , Type (TypeApplication (TypeConstructor ref)
                                         (TypeVariable 0)) ] )
      , ("Nil", []) ]
