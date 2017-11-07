module Epic.ResolverTest
  ( resolverTests
  ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Except

import           Data.Either
import           Data.List
import qualified Data.Text as T

import           Epic.Language
import           Epic.Resolver

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           TestHelpers

newtype RefForceEq a = RefForceEq (Ref a)

instance Eq a => Eq (RefForceEq a) where
  RefForceEq (Ref mname name def) == RefForceEq (Ref mname' name' def') =
    mname == mname' && name == name' && def == def'

instance Show a => Show (RefForceEq a) where
  show (RefForceEq (Ref mname name def)) =
    "(Ref " ++ show mname ++ " " ++ show name ++ " (" ++ show def ++ "))"

resolverTests :: TestTree
resolverTests = testGroup "Epic.Resolver"
  [ resolveTypeDefinitionsTests, resolveTypeDefinitionTests, resolveTypeTests
  , resolveLocalTypeTests, resolveDefinitionsTests, resolveDefinitionTests
  , resolveTermTests, resolvePatternTests, resolveReferenceTests
  , resolveConstructorReferenceTests, reorderModulesTests
  ]

resolveTypeDefinitionsTests :: TestTree
resolveTypeDefinitionsTests = testGroup "resolveTypeDefinitions"
  [ testCase "resolves the type definitions of a module keeping them in the\
             \ same order" $ do
      let env   = ResolverEnvironment [] (ModuleName ["A"]) [] []
          ldef1 = TypeDefinition "Type1" [()] [("Cons1", [PrimTypeInt])]
          ldef2 = TypeDefinition "Type2" [()] [("Cons2", [PrimTypeInt])]
          def1  = TypeDefinition "Type1" [()] [("Cons1", [FQType PrimTypeInt])]
          def2  = TypeDefinition "Type2" [()] [("Cons2", [FQType PrimTypeInt])]
          _mod  = Module (ModuleName ["A"]) Nothing [] [ldef1, ldef2] []
          env'  = env & localTypes .~ [def2, def1]
      runExcept (resolveTypeDefinitions env _mod) @?= Right (env', [def1, def2])
  ]

resolveTypeDefinitionTests :: TestTree
resolveTypeDefinitionTests = testGroup "resolveTypeDefinition"
  [ testCase "resolves a type definition recursively and adds it to the\
             \ environment" $ do
      let env = ResolverEnvironment [] (ModuleName ["A"]) [] [optionDef]
          ltype = TypeApplication (TypeConstructor (NameReference "Option"))
                                  PrimTypeInt
          typ   = FQType (TypeApplication
                          (TypeConstructor (Ref (ModuleName ["A"]) "Option"
                                                optionDef))
                          PrimTypeInt)
          ldef  = TypeDefinition "Type" [()] [("Something", [ltype])]
          def   = TypeDefinition "Type" [()] [("Something", [typ])]
          env'  = env & localTypes .~ [def, optionDef]
      runExcept (resolveTypeDefinition env ldef) @?= Right (env', def)
  ]

resolveTypeTests :: TestTree
resolveTypeTests = testGroup "resolveType"
  [ testCase "resolves a type" $ do
      let env   = ResolverEnvironment [] (ModuleName ["A"]) [] [optionDef]
          ltype = TypeApplication (TypeConstructor (NameReference "Option"))
                                  PrimTypeInt
          typ   = FQType (TypeApplication
                          (TypeConstructor (Ref (ModuleName ["A"]) "Option"
                                                optionDef))
                          PrimTypeInt)
      runExcept (resolveType env ltype) @?= Right typ
  ]

resolveLocalTypeTests :: TestTree
resolveLocalTypeTests = testGroup "resolveLocalType"
  [ testCase "resolves a NameReference defined locally" $ do
      let def  = TypeDefinition "Type"  [()] [("Something", [])]
          def' = TypeDefinition "Wrong" [()] [("Something", [])]
          imported = Module (ModuleName ["B"]) Nothing [] [def'] []
          env = ResolverEnvironment [imported] (ModuleName ["A"]) [] [def]
          result = fmap RefForceEq $ resolveLocalType env (NameReference "Type")
      runExcept result
        @?= Right (RefForceEq (Ref (ModuleName ["A"]) "Type" def))

  , testCase "resolves a NameReference defined in an imported module" $ do
      let def = TypeDefinition "Type" [()] [("Something", [])]
          imported = Module (ModuleName ["B"]) Nothing [] [def] []
          env = ResolverEnvironment [imported] (ModuleName ["A"]) [] []
          result = fmap RefForceEq $ resolveLocalType env (NameReference "Type")
      runExcept result
        @?= Right (RefForceEq (Ref (ModuleName ["B"]) "Type" def))

  , testCase "resolves a FQReference" $ do
      let def = TypeDefinition "Type" [()] [("Something", [])]
          imported = Module (ModuleName ["B"]) Nothing [] [def] []
          env = ResolverEnvironment [imported] (ModuleName ["A"]) [] []
          result = fmap RefForceEq $ resolveLocalType env $
            FQReference (ModuleName ["B"]) "Type"
      runExcept result
        @?= Right (RefForceEq (Ref (ModuleName ["B"]) "Type" def))

  , testCase "fails if the NameReference can't be found" $ do
      let imported = Module (ModuleName ["B"]) Nothing [] [] []
          env = ResolverEnvironment [imported] (ModuleName ["A"]) [] []
          result = fmap RefForceEq $
            resolveLocalType env (NameReference "Type")
      assertBool "should fail" (isLeft (runExcept result))

  , testCase "fails if the FQReference can't be found" $ do
      let imported = Module (ModuleName ["B"]) Nothing [] [] []
          env = ResolverEnvironment [imported] (ModuleName ["A"]) [] []
          result = fmap RefForceEq $ resolveLocalType env $
            FQReference (ModuleName ["B"]) "Type"
      assertBool "should fail" (isLeft (runExcept result))
  ]

resolveDefinitionsTests :: TestTree
resolveDefinitionsTests = testGroup "resolveDefinitions"
  [ testCase "resolves the definitions of a module keeping them in the same\
             \  order" $ do
      let ldef1 = TermDefinition "someConstant"  (PrimInt 1) (Just PrimTypeInt)
          ldef2 = TermDefinition "otherConstant" (PrimInt 2) (Just PrimTypeInt)
          env   = ResolverEnvironment [] (ModuleName ["A"]) [] []
          def1  = FQDefinition (TermDefinition "someConstant" (PrimInt 1)
                                               (Just (FQType PrimTypeInt)))
          def2  = FQDefinition (TermDefinition "otherConstant" (PrimInt 2)
                                               (Just (FQType PrimTypeInt)))
          env'  = env & localBindings .~ [def2, def1]
          _mod  = Module (ModuleName ["A"]) Nothing [] [] [ldef1, ldef2]
      runExcept (resolveDefinitions env _mod) @?= Right (env', [def1, def2])
  ]

resolveDefinitionTests :: TestTree
resolveDefinitionTests = testGroup "resolveDefinition"
  [ testCase "resolves a definition recursively and adds it to the\
             \ environment" $ do
      let ldef = TermDefinition "someConstant" (PrimInt 1) (Just PrimTypeInt)
          env  = ResolverEnvironment [] (ModuleName ["A"]) [] []
          def  = FQDefinition (TermDefinition "someConstant" (PrimInt 1)
                                              (Just (FQType PrimTypeInt)))
          env' = env & localBindings %~ (def :)
      runExcept (resolveDefinition env ldef) @?= Right (env', def)
  ]

resolveTermTests :: TestTree
resolveTermTests = testGroup "resolveTerm"
  [ testCase "resolves a term recursively" $ do
      let someConstant =
            FQDefinition (TermDefinition "someConstant" (PrimInt 1) Nothing)
          env = ResolverEnvironment [] (ModuleName ["A"]) [someConstant] []
          lterm = Application (Reference (NameReference "someConstant"))
                              (PrimInt 2)
          fqterm = Application (Reference (Ref (ModuleName ["A"]) "someConstant"
                                               someConstant))
                               (PrimInt 2)
      runExcept (resolveTerm env lterm) @?= Right fqterm
  ]

resolvePatternTests :: TestTree
resolvePatternTests =
  let env = ResolverEnvironment [] (ModuleName ["A"]) [] [optionDef]
  in testGroup "resolvePattern"
       [ testCase "does nothing for WildcardPattern" $ do
           runExcept (resolvePattern env WildcardPattern)
             @?= Right WildcardPattern

       , testCase "does nothing for VariablePattern" $ do
           runExcept (resolvePattern env (VariablePattern "x"))
             @?= Right (VariablePattern "x")

       , testCase "resolves the constructor and recurses for\
                  \ ConstructorPattern" $ do
           let pat = ConstructorPattern (NameReference "Some") "Some"
                                        [VariablePattern "x"]
           runExcept (resolvePattern env pat)
             @?= Right (ConstructorPattern
                        (Ref (ModuleName ["A"]) "Option" optionDef)
                        "Some" [VariablePattern "x"])
       ]

resolveReferenceTests :: TestTree
resolveReferenceTests = testGroup "resolveReference"
  [ testCase "resolves a NameReference defined locally" $ do
      let def  = FQDefinition (ForeignDefinition "something"
                                                 (FQType PrimTypeInt))
          def' = FQDefinition (ForeignDefinition "something"
                                                 (FQType PrimTypeBool))
          imported = Module (ModuleName ["B"]) Nothing [] [] [def']
          env = ResolverEnvironment [imported] (ModuleName ["A"]) [def] []
          result = fmap RefForceEq $
            resolveReference env (NameReference "something")
      runExcept result
        @?= Right (RefForceEq (Ref (ModuleName ["A"]) "something" def))

  , testCase "resolves a NameReference defined in an imported module" $ do
      let def = FQDefinition (ForeignDefinition "something"
                                                (FQType PrimTypeInt))
          imported = Module (ModuleName ["B"]) Nothing [] [] [def]
          env = ResolverEnvironment [imported] (ModuleName ["A"]) [] []
          result = fmap RefForceEq $
            resolveReference env (NameReference "something")
      runExcept result
        @?= Right (RefForceEq (Ref (ModuleName ["B"]) "something" def))

  , testCase "resolves a FQReference" $ do
      let def = FQDefinition (ForeignDefinition "something"
                                                (FQType PrimTypeInt))
          imported = Module (ModuleName ["B"]) Nothing [] [] [def]
          env = ResolverEnvironment [imported] (ModuleName ["A"]) [] []
          result = fmap RefForceEq $ resolveReference env $
            FQReference (ModuleName ["B"]) "something"
      runExcept result
        @?= Right (RefForceEq (Ref (ModuleName ["B"]) "something" def))

  , testCase "fails if the NameReference can't be found" $ do
      let imported = Module (ModuleName ["B"]) Nothing [] [] []
          env = ResolverEnvironment [imported] (ModuleName ["A"]) [] []
          result = fmap RefForceEq $
            resolveReference env (NameReference "something")
      assertBool "should fail" (isLeft (runExcept result))

  , testCase "fails if the FQReference can't be found" $ do
      let imported = Module (ModuleName ["B"]) Nothing [] [] []
          env = ResolverEnvironment [imported] (ModuleName ["A"]) [] []
          result = fmap RefForceEq $ resolveReference env $
            FQReference (ModuleName ["B"]) "something"
      assertBool "should fail" (isLeft (runExcept result))
  ]

resolveConstructorReferenceTests :: TestTree
resolveConstructorReferenceTests = testGroup "resolveConstructorReference"
  [ testCase "resolves a NameReference defined locally" $ do
      let def  = TypeDefinition "Type"  [()] [("Something", [])]
          def' = TypeDefinition "Wrong" [()] [("Something", [])]
          imported = Module (ModuleName ["B"]) Nothing [] [def'] []
          env = ResolverEnvironment [imported] (ModuleName ["A"]) [] [def]
          result = fmap RefForceEq $
            resolveConstructorReference env (NameReference "Something")
      runExcept result
        @?= Right (RefForceEq (Ref (ModuleName ["A"]) "Type" def))

  , testCase "resolves a NameReference defined in an imported module" $ do
      let def = TypeDefinition "Type" [()] [("Something", [])]
          imported = Module (ModuleName ["B"]) Nothing [] [def] []
          env = ResolverEnvironment [imported] (ModuleName ["A"]) [] []
          result = fmap RefForceEq $
            resolveConstructorReference env (NameReference "Something")
      runExcept result
        @?= Right (RefForceEq (Ref (ModuleName ["B"]) "Type" def))

  , testCase "resolves a FQReference" $ do
      let def = TypeDefinition "Type" [()] [("Something", [])]
          imported = Module (ModuleName ["B"]) Nothing [] [def] []
          env = ResolverEnvironment [imported] (ModuleName ["A"]) [] []
          result = fmap RefForceEq $ resolveConstructorReference env $
            FQReference (ModuleName ["B"]) "Something"
      runExcept result
        @?= Right (RefForceEq (Ref (ModuleName ["B"]) "Type" def))

  , testCase "fails if the NameReference can't be found" $ do
      let imported = Module (ModuleName ["B"]) Nothing [] [] []
          env = ResolverEnvironment [imported] (ModuleName ["A"]) [] []
          result = fmap RefForceEq $
            resolveConstructorReference env (NameReference "Something")
      assertBool "should fail" (isLeft (runExcept result))

  , testCase "fails if the FQReference can't be found" $ do
      let imported = Module (ModuleName ["B"]) Nothing [] [] []
          env = ResolverEnvironment [imported] (ModuleName ["A"]) [] []
          result = fmap RefForceEq $ resolveConstructorReference env $
            FQReference (ModuleName ["B"]) "Something"
      assertBool "should fail" (isLeft (runExcept result))
  ]

reorderModulesTests :: TestTree
reorderModulesTests = testGroup "reorderModules"
  [ testProperty "reorders modules so that modules only depend on modules on\
                 \ the left" reorderModulesLeftDependencies

  , testCase "detects circular dependency" $ do
      let modA = Module (ModuleName ["A"]) Nothing [ModuleName ["B"]] [] []
          modB = Module (ModuleName ["B"]) Nothing [ModuleName ["A"]] [] []
      runExcept (reorderModules ([modA, modB] :: [Module]))
        @?= Left "circular dependency detected"
  ]

reorderModulesLeftDependencies :: Property
reorderModulesLeftDependencies = property $ do
    mnames <- fmap nub $ forAll $ Gen.list (Range.linear 1 10) moduleNameGen
    let step acc mname = do
          imps <- forAll $
            if null acc
              then return []
              else Gen.list (Range.linear 0 (length acc))
                            (Gen.element (map (view moduleName) acc))
          let _mod = Module mname Nothing imps [] []
          return $ _mod : acc
    modules  <- foldM step [] mnames

    shuffled <- forAll $ Gen.shuffle modules
    case runExcept (reorderModules shuffled) of
      Left err -> annotate (T.unpack err) >> failure
      Right ordered -> checkOrder $ reverse ordered

  where
    checkOrder :: [Module] -> PropertyT IO ()
    checkOrder [] = success
    checkOrder (_mod : modules) = do
      let mnames = map (view moduleName) modules
      forM_ (_mod ^. imports) $ \mname -> unless (mname `elem` mnames) failure
      checkOrder modules

optionDef :: TypeDefinition FQType ()
optionDef = TypeDefinition "Option" [()]
  [ ("None", [])
  , ("Some", [FQType (TypeVariable 0)]) ]
