{-# LANGUAGE RankNTypes #-}

module Epic.TypeCheckerTest where

import           Control.Lens
import           Control.Monad.State

import           Data.Either
import           Data.Functor.Foldable
import qualified Data.Text as T

import           Epic.Language
import           Epic.TypeChecker.Internal

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           TestHelpers

typeCheckerTests :: TestTree
typeCheckerTests = testGroup "Epic.TypeChecker"
  [ toMetaTests, substMetaTests, fromMetaTypeTests, fromMetaKindTests
  , typeCheckDefinitionTests, typeOfTests, unifyTests, removeMissingPatternTests
  , substDeBruijnIndexTests, bumpTypeIndexFromTests, constructorTypeTests
  , kindCheckTypeDefinitionTests
  ]

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
  [ testCase "substitutes meta variables by the given structure" $ do
      substMeta 3 (Fix (MetaBase (Left ("replacement" :: String))))
                  (Fix
                   (MetaBase (Right (Fix (MetaBase (Right (MetaIndex 3)))))))
        @?= Fix (MetaBase
                 (Right
                  (Fix
                   (MetaBase (Right (Fix (MetaBase (Left "replacement"))))))))
  ]

fromMetaTypeTests :: TestTree
fromMetaTypeTests = testGroup "fromMetaType"
  [ testCase "substitutes meta indices with their corresponding types" $ do
      fromMetaType [(1, PrimTypeIntM), (2, PrimTypeBoolM)]
                   (FunctionTypeM (MetaIndex 1) (MetaIndex 2))
        @?= Right (Type (FunctionType PrimTypeInt PrimTypeBool))

  , testCase "substitutes inside the replacements as well (sensitive to\
             \ order)" $ do
      fromMetaType [ (2, PrimTypeIntM)
                   , (1, FunctionTypeM (MetaIndex 2) (MetaIndex 2)) ]
                   (MetaIndex 1)
        @?= Right (Type (FunctionType PrimTypeInt PrimTypeInt))

  , testCase "replaces missing meta indices by type variables" $ do
      fromMetaType [] (FunctionTypeM (MetaIndex 1)
                       (FunctionTypeM (MetaIndex 1) (MetaIndex 2)))
        @?= Right (Type (UniversalType (UniversalType
                         (FunctionType (TypeVariable 1)
                          (FunctionType (TypeVariable 1) (TypeVariable 0))))))
  ]

fromMetaKindTests :: TestTree
fromMetaKindTests = testGroup "fromMetaKind"
  [ testCase "substitutes meta indices with their corresponding kinds" $
      fromMetaKind [(1, ArrowM StarM StarM)]
                   (ArrowM (MetaIndex 1) (MetaIndex 1))
        @?= Arrow (Arrow Star Star) (Arrow Star Star)

  , testCase "substitutes inside the replacements as well (sensitive to\
             \ order)" $
      fromMetaKind [ (2, ArrowM StarM StarM)
                   , (1, ArrowM (MetaIndex 2) (MetaIndex 2)) ] (MetaIndex 1)
        @?= Arrow (Arrow Star Star) (Arrow Star Star)

  , testCase "forces unknown kinds to *" $
      fromMetaKind [] (ArrowM (MetaIndex 1) (MetaIndex 2)) @?= Arrow Star Star
  ]

typeCheckDefinitionTests :: TestTree
typeCheckDefinitionTests = testGroup "typeCheckDefinition"
  [ testCase "checks and caches the type of a term definition without\
             \ signature" $ do
      let fqdef = FQDefinition (TermDefinition "someValue" (PrimInt 3) Nothing)
      case runStateT (typeCheckDefinition (ModuleName ["A"]) fqdef)
                     emptyTypeCheckerState of
        Left err -> assertFailure $ T.unpack err
        Right (def, st) -> do
          st ^. typedDefinitions @?= [(((ModuleName ["A"]), "someValue"), def)]
          def @?= Definition (TermDefinition "someValue" (PrimInt 3)
                                             (Type PrimTypeInt))

  , testCase "checks and caches the type of a term definition with a\
             \ signature" $ do
      let fqdef = FQDefinition (TermDefinition "someFunc"
                                (Abstraction Nothing (Variable 0))
                                (Just (FQType
                                       (FunctionType PrimTypeInt PrimTypeInt))))
      case runStateT (typeCheckDefinition (ModuleName ["A"]) fqdef)
                     emptyTypeCheckerState of
        Left err -> assertFailure $ T.unpack err
        Right (def, st) -> do
          st ^. typedDefinitions @?= [(((ModuleName ["A"]), "someFunc"), def)]
          def @?= Definition (TermDefinition "someFunc"
                              (Abstraction Nothing (Variable 0))
                              (Type (FunctionType PrimTypeInt PrimTypeInt)))

  , testCase "caches and returns the type of a foreign import" $ do
      let fqdef = FQDefinition (ForeignDefinition "foreignCall"
                                (FQType PrimTypeInt))
      case runStateT (typeCheckDefinition (ModuleName ["A"]) fqdef)
                     emptyTypeCheckerState of
        Left err -> assertFailure $ T.unpack err
        Right (def, st) -> do
          st ^. typedDefinitions @?= [((ModuleName ["A"], "foreignCall"), def)]
          def @?= Definition (ForeignDefinition "foreignCall"
                              (Type PrimTypeInt))

  , testCase "fails if the signature is less specific than the inferred\
             \ type" $ do
      let fqdef = FQDefinition (TermDefinition "someValue" (PrimInt 1)
                                (Just (FQType
                                       (UniversalType (TypeVariable 0)))))
      case runStateT (typeCheckDefinition (ModuleName ["A"]) fqdef)
                     emptyTypeCheckerState of
        Left _ -> return ()
        Right (def, _) -> assertString $ "should fail, returned " ++ show def
  ]

typeOfTests :: TestTree
typeOfTests = testGroup "typeOf"
  [ testProperty "returns the type of a term together with this term where all\
                 \ references have been typechecked and substituted" $
      property $ do
        (fqterm, result) <- forAll typedTermGen
        evalStateT (typeOf fqterm) emptyTypeCheckerState === Right result

  , testCase "generalises with universal types when it has no information about\
             \ some types" $ do
      let fqterm = Abstraction Nothing
                               (Abstraction Nothing
                                (Application (Variable 1) (Variable 0)))
          term = Abstraction Nothing
                             (Abstraction Nothing
                              (Application (Variable 1) (Variable 0)))
          typ = Type (UniversalType
                      (UniversalType
                       (FunctionType
                        (FunctionType (TypeVariable 1) (TypeVariable 0))
                        (FunctionType (TypeVariable 1)
                                      (TypeVariable 0)))))

      evalStateT (typeOf fqterm) emptyTypeCheckerState @?= Right (term, typ)

  , testCase "specialises usage of references" $ do
      let idTerm = Abstraction Nothing (Variable 0)
          idTyp  = Type (UniversalType
                         (FunctionType (TypeVariable 0) (TypeVariable 0)))
          idFQDef = FQDefinition (TermDefinition "id"
                                  (error "should not use this value") Nothing)
          idDef  = Definition (TermDefinition "id" idTerm idTyp)

          fqterm = Abstraction (Just (FQType PrimTypeInt))
                               (Application
                                (Reference
                                 (Ref (ModuleName ["A"]) "id" idFQDef))
                                (Variable 0))

          term = Abstraction Nothing
                             (Application
                              (Reference (Ref (ModuleName ["A"]) "id" idDef))
                              (Variable 0))
          typ = Type (FunctionType PrimTypeInt PrimTypeInt)
          st = emptyTypeCheckerState & typedDefinitions .~
                 [((ModuleName ["A"], "id"), idDef)]

      evalStateT (typeOf fqterm) st @?= Right (term, typ)

  , testCase "checks the type of pattern matches" $ do
      let tyFQDef = TypeDefinition "SomeType" [()]
            [("One", [FQType PrimTypeInt]), ("Two", [FQType (TypeVariable 0)])]
          tyDef = TypeDefinition "SomeType" [Star]
            [("One", [Type PrimTypeInt]), ("Two", [Type (TypeVariable 0)])]
          tyFQRef = Ref (ModuleName ["A"]) "SomeType" tyFQDef
          tyRef = Ref (ModuleName ["A"]) "SomeType" tyDef

          fqterm = Abstraction Nothing $ PatternMatch (Variable 0)
            [ ( ConstructorPattern tyFQRef "One" [WildcardPattern]
              , PrimInt 1 )
            , ( ConstructorPattern tyFQRef "Two" [VariablePattern "x"]
              , Variable 0 )
            ]

          term = Abstraction Nothing $ PatternMatch (Variable 0)
            [ (ConstructorPattern tyRef "One" [WildcardPattern],     PrimInt 1)
            , (ConstructorPattern tyRef "Two" [VariablePattern "x"], Variable 0)
            ]

          typ = Type (FunctionType
                      (TypeApplication (TypeConstructor tyRef) PrimTypeInt)
                      PrimTypeInt)

      evalStateT (typeOf fqterm) emptyTypeCheckerState @?= Right (term, typ)

  , testCase "accepts a generic function where a more specific one was\
             \ expected" $ do
      let idTerm = Abstraction Nothing (Variable 0)
          idTyp  = Type (UniversalType
                         (FunctionType (TypeVariable 0) (TypeVariable 0)))
          idFQDef = FQDefinition (TermDefinition "id"
                                  (error "should not use this value") Nothing)
          idDef  = Definition (TermDefinition "id" idTerm idTyp)

          fTerm = Abstraction Nothing (Application (Variable 0) (PrimInt 42))
          fTyp  = Type (FunctionType (FunctionType PrimTypeInt PrimTypeInt)
                                     PrimTypeInt)
          fFQDef = FQDefinition (TermDefinition "f"
                                  (error "should not use this value") Nothing)
          fDef  = Definition (TermDefinition "f" fTerm fTyp)

          fqterm = Application (Reference (Ref (ModuleName ["A"]) "f"  fFQDef))
                               (Reference (Ref (ModuleName ["A"]) "id" idFQDef))
          term   = Application (Reference (Ref (ModuleName ["A"]) "f"  fDef))
                               (Reference (Ref (ModuleName ["A"]) "id" idDef))

          st = emptyTypeCheckerState & typedDefinitions .~
                 [ ((ModuleName ["A"], "id"), idDef)
                 , ((ModuleName ["A"], "f"),  fDef) ]

      evalStateT (typeOf fqterm) st @?= Right (term, Type PrimTypeInt)
  ]

unifyTests :: TestTree
unifyTests = testGroup "unify"
  [ testCase "rejects forall a. Int -> a as a subtype of forall a. a -> a" $ do
      let typeA = UniversalTypeM (FunctionTypeM PrimTypeIntM (TypeVariableM 0))
          typeB = UniversalTypeM (FunctionTypeM (TypeVariableM 0)
                                                (TypeVariableM 0))
      assertBool "should fail"
                 (isLeft (evalStateT (unify typeA typeB)
                                     emptyTypeCheckerState))

  ]

removeMissingPatternTests :: TestTree
removeMissingPatternTests = testGroup "removeMissingPattern" $
  let mkTypeDef ref = TypeDefinition "SomeType" [Star]
        [ ("One", [Type PrimTypeInt, Type (TypeVariable 0)])
        , ("Two", [])
        , ("Three", [ Type PrimTypeBool
                    , Type (TypeApplication (TypeConstructor ref)
                                            (TypeVariable 0))
                    ])
        ]
      typeRef = Ref (ModuleName ["A"]) "SomeType" (mkTypeDef typeRef)

  in [ testCase "returns Nothing if nothing is missing" $ do
         removeMissingPattern (MissingConstructors []) undefined @?= Nothing

     , testCase "removes every missing patterns if a WildcardPattern\
                \ is given" $ do
         removeMissingPattern MissingAll WildcardPattern
           @?= Just (MissingConstructors [])
         removeMissingPattern (MissingConstructors [("C", [])]) WildcardPattern
           @?= Just (MissingConstructors [])

     , testCase "leaves missing constructors if given a ConstructorPattern on a\
                \ MissingAll" $ do
         removeMissingPattern MissingAll (ConstructorPattern typeRef "One"
                                          [WildcardPattern, WildcardPattern])
           @?= Just (MissingConstructors
                     [ ("Two", [])
                     , ("Three", [MissingAll, MissingAll]) ])
         removeMissingPattern MissingAll (ConstructorPattern typeRef "Two" [])
           @?= Just (MissingConstructors
                     [ ("One", [MissingAll, MissingAll])
                     , ("Three", [MissingAll, MissingAll]) ])

     , testCase "recurses into subpatterns" $ do
         removeMissingPattern MissingAll (ConstructorPattern typeRef "Three"
                                          [ WildcardPattern
                                          , ConstructorPattern typeRef "Two" []
                                          ])
           @?= Just (MissingConstructors
                     [ ("Three", [ MissingConstructors []
                                 , MissingConstructors
                                     [ ("One", [MissingAll, MissingAll])
                                     , ("Three", [MissingAll, MissingAll])
                                     ]
                                 ])
                     , ("One", [MissingAll, MissingAll])
                     , ("Two", [])
                     ])

     , testCase "returns Nothing when the constructor is not missing\
                \ anymore" $ do
         removeMissingPattern (MissingConstructors [("Two", [])])
                              (ConstructorPattern typeRef "One"
                               [WildcardPattern, WildcardPattern])
           @?= Nothing

     ]

substDeBruijnIndexTests :: TestTree
substDeBruijnIndexTests = testGroup "substDeBruijnIndex"
  [ testCase "replaces some type variables" $ do
      substDeBruijnIndex 0 [(0, PrimTypeIntM), (1, PrimTypeBoolM)]
                         (FunctionTypeM (TypeVariableM 0) (TypeVariableM 1))
        @?= FunctionTypeM PrimTypeIntM PrimTypeBoolM

  , testCase "takes forall into account" $ do
      substDeBruijnIndex 0 [(0, PrimTypeIntM), (1, PrimTypeBoolM)]
                         (UniversalTypeM (TypeVariableM 1))
        @?= UniversalTypeM PrimTypeIntM

  ]

bumpTypeIndexFromTests :: TestTree
bumpTypeIndexFromTests = testGroup "bumpTypeIndexFrom"
  [ testCase "increments type variables if they are above the threshold" $
      bumpTypeIndexFrom 3 (FunctionTypeM (TypeVariableM 2) (TypeVariableM 3))
        @?= FunctionTypeM (TypeVariableM 2) (TypeVariableM 4)

  , testCase "increments the threshold on universal types" $
      bumpTypeIndexFrom 1 (UniversalTypeM (TypeVariableM 2))
        @?= UniversalTypeM (TypeVariableM 3)
  ]

constructorTypeTests :: TestTree
constructorTypeTests = testGroup "constructorType"
  [ testCase "returns the type of a given constructor of a given type" $ do
      let typeDef = TypeDefinition "SomeType" [Star]
            [("One", [Type PrimTypeInt]), ("Two", [Type (TypeVariable 0)])]
          typeRef = Ref (ModuleName ["A"]) "SomeType" typeDef
      constructorType (ModuleName ["A"]) typeDef "Two"
        @?= Right (Type (UniversalType
                         (FunctionType (TypeVariable 0)
                          (TypeApplication (TypeConstructor typeRef)
                                           (TypeVariable 0)))))
  ]

kindCheckTypeDefinitionTests :: TestTree
kindCheckTypeDefinitionTests = testGroup "kindCheckTypeDefinition"
  [ testCase "returns a kinded type definition" $ do
      let typeFQDef = TypeDefinition "SomeType" [(), ()]
            [( "SomeConstructor", [FQType (TypeApplication (TypeVariable 1)
                                                           (TypeVariable 0))] )]
          typeDef = TypeDefinition "SomeType" [Arrow Star Star, Star]
            [( "SomeConstructor", [Type (TypeApplication (TypeVariable 1)
                                                         (TypeVariable 0))] )]
      evalStateT (kindCheckTypeDefinition (ModuleName ["A"]) typeFQDef)
                 emptyTypeCheckerState
        @?= Right typeDef

  ]

typedTermGen :: Gen (FQTerm, (Term, Type))
typedTermGen = Gen.choice
  [ boolTermGen True True, intTermGen True True
  , ifthenelseGen typedTermGen
  , applicationGen typedTermGen
  , do
      sub <- typedTermGen
      input <- Gen.element [ (FQType PrimTypeInt, Type PrimTypeInt)
                           , (FQType PrimTypeBool, Type PrimTypeBool) ]
      abstractionGen input sub
  ]

boolTermGen :: Bool -> Bool -> Gen (FQTerm, (Term, Type))
boolTermGen doIf doApp = Gen.sized $ \size -> Gen.choice $
  [ return (PrimBool True,  (PrimBool True,  Type PrimTypeBool))
  , return (PrimBool False, (PrimBool False, Type PrimTypeBool)) ]
  ++ (if size > 1 && doIf
        then [Gen.small (ifthenelseGen (boolTermGen False doApp))]
        else [])
  ++ (if size > 1 && doApp
        then [Gen.small (applicationGen (boolTermGen doIf False))]
        else [])

intTermGen :: Bool -> Bool -> Gen (FQTerm, (Term, Type))
intTermGen doIf doApp = Gen.sized $ \size -> Gen.choice $
  [ Gen.int (Range.linear 0 1000) >>= \i ->
      return (PrimInt i, (PrimInt i, Type PrimTypeInt)) ]
  ++ (if size > 1 && doIf
        then [Gen.small (ifthenelseGen (intTermGen False doApp))]
        else [])
  ++ (if size > 1 && doApp
        then [Gen.small (applicationGen (intTermGen doIf False))]
        else [])

ifthenelseGen :: Gen (FQTerm, (Term, Type)) -> Gen (FQTerm, (Term, Type))
ifthenelseGen branchGen = do
  (condFQTerm, (condTerm, _))  <- boolTermGen False True
  (thenFQTerm, (thenTerm, t))  <- branchGen
  (elseFQTerm, (elseTerm, t')) <- branchGen
  unless (t == t') Gen.discard
  return (  IfThenElse condFQTerm thenFQTerm elseFQTerm
         , (IfThenElse condTerm   thenTerm   elseTerm, t) )

-- It is the responsibility of the caller to ensure that the types are correct.
applicationGen :: Gen (FQTerm, (Term, Type)) -> Gen (FQTerm, (Term, Type))
applicationGen resultGen = do
  result@(_, (_, typ)) <- resultGen
  Gen.choice
     [ do (paramFQTerm, (paramTerm, _)) <- boolTermGen True True
          (absFQTerm, (absTerm, _)) <-
            abstractionGen (FQType PrimTypeBool, Type PrimTypeBool)
                           result
          return (  Application absFQTerm paramFQTerm
                 , (Application absTerm   paramTerm, typ) )
     ]

-- No universal types as it doesn't play well with recursion.
abstractionGen :: (FQType, Type) -> (FQTerm, (Term, Type))
               -> Gen (FQTerm, (Term, Type))
abstractionGen (fqtyp, Type typ) (bodyFQTerm, (bodyTerm, Type bodyType)) = do
  let finalType = Type (FunctionType typ bodyType)
  return (  Abstraction (Just fqtyp) bodyFQTerm
         , (Abstraction Nothing bodyTerm, finalType) )
