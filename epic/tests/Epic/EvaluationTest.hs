module Epic.EvaluationTest
  ( evaluationTests
  ) where

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
      evalWHNF [] (BaseTerm term) @?= BaseTerm (PrimInt 42)

  , testCase "evaluates a reference to the value it's pointing to" $ do
      let idTerm = Abstraction Nothing (Variable 0)
          idType = Type (UniversalType
                         (FunctionType (TypeVariable 0) (TypeVariable 0)))
          idDef = Definition (TermDefinition "id" idTerm idType)
          idRef = Ref (ModuleName ["A"]) "id" idDef

          term = Application (Reference idRef) (PrimInt 42)

      evalWHNF [] (BaseTerm term) @?= BaseTerm (PrimInt 42)

  , testCase "evaluates a foreign reference to the corresponding Haskell\
              \ function" $ do
      let inc (BaseTerm (PrimInt x)) = BaseTerm (PrimInt (x+1))
          inc _ = undefined

          incType = Type (FunctionType PrimTypeInt PrimTypeInt)
          incDef  = Definition (ForeignDefinition "inc" incType)
          idRef   = Ref (ModuleName ["A"]) "inc" incDef

          term = Application (Reference idRef) (PrimInt 42)

      evalWHNF [("inc", inc)] (BaseTerm term) @?= BaseTerm (PrimInt 43)

  , testCase "evaluates an application to a constructor" $ do
      let typeDef = TypeDefinition "SomeType" []
                                   [("SomeConstructor", [Type PrimTypeInt])]
          typeRef = Ref (ModuleName ["A"]) "SomeType" typeDef

          term = Application (ConstructorReference typeRef "SomeConstructor")
                             (PrimInt 100)

      evalWHNF [] (BaseTerm term) @?= Constructor 0 [BaseTerm (PrimInt 100)]

  , testCase "evaluates the correct branch of an if-then-else" $ do
      let term b = IfThenElse (PrimBool b) (PrimInt 1) (PrimInt 2)
      evalWHNF [] (BaseTerm (term True))  @?= BaseTerm (PrimInt 1)
      evalWHNF [] (BaseTerm (term False)) @?= BaseTerm (PrimInt 2)

  , testCase "evaluates an application to fix recursively" $ do
      -- fix (\f -> \b -> if b then 0 else f true)
      let func = Application FixTerm
                 (Abstraction Nothing
                  (Abstraction Nothing
                   (IfThenElse (Variable 0)
                    (PrimInt 0)
                    (Application (Variable 1) (PrimBool True)))))
          term = Application func (PrimBool False)

      evalWHNF [] (BaseTerm term) @?= BaseTerm (PrimInt 0)

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

      evalWHNF [] (BaseTerm term) @?= BaseTerm (PrimInt 1337)
  ]

matchPatternsTests :: TestTree
matchPatternsTests = testGroup "matchPatterns"
  [ testCase "returns the first branch that the term matches" $ do
      matchPatterns [] []
                    (Constructor 0 [ BaseTerm (PrimBool True)
                                   , Constructor 1 [] ])
                    [ (ConstructorPattern listTypeRef "Nil" [], PrimInt 1)
                    , (WildcardPattern, PrimInt 2) ]
        @?= (PrimInt 2, [])

  , testCase "returns the values to assign to the variables" $ do
      matchPatterns [] []
                    (Constructor 0 [ Constructor 0 [ Constructor 1 []
                                                   , BaseTerm (PrimInt 2) ]
                                   , BaseTerm (PrimInt 1) ])
                    [ (ConstructorPattern listTypeRef "Nil" [], PrimBool False)
                    , (ConstructorPattern listTypeRef "Cons"
                       [ VariablePattern "x"
                       , ConstructorPattern listTypeRef "Cons"
                           [ VariablePattern "y"
                           , VariablePattern "z" ]], PrimBool True) ]
        @?= (PrimBool True, [ Constructor 1 []
                            , BaseTerm (PrimInt 2)
                            , BaseTerm (PrimInt 1) ])
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
