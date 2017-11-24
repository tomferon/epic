module Epic.ParserTest
  ( parserTests
  ) where

import           Control.Monad

import           Data.Attoparsec.Text
import           Data.Either
import           Data.List
import           Data.Monoid
import qualified Data.Text as T

import           Epic.Language
import           Epic.Parser.Internal

import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           TestHelpers

parserTests :: TestTree
parserTests = testGroup "Epic.Parser"
  [ moduleParserTests, termParserTests, typeParserTests, patternParserTests
  , moduleNameParserTests
  -- Helpers for moduleParser
  , exportsParserTests, importParserTests, termDefinitionTests, signatureTests
  , foreignImportTests, typeDefinitionTests
  -- Helpers for termParser
  , variableOrReferenceTests, constructorReferenceTests, abstractionTests
  , ifthenelseTests, applicationTests, patternMatchTests, operationTests
  , intTermTests, charTermTests, stringTermTests
  -- Helpers for typeParser
  , forallTests, functionTests, typeApplicationTests
  -- Generic helpers
  , identifierTests, constructorTests, operatorTests, commentTests, sepTests
  , sep1Tests, selectTests, findInContextTests
  ]

moduleParserTests :: TestTree
moduleParserTests = testGroup "moduleParser"
  [ testProperty "parses a module" $ property $ do
      (txt, _mod) <- forAll moduleGen
      parseOnly moduleParser txt === Right _mod
  ]

termParserTests :: TestTree
termParserTests = testGroup "termParser"
  [ testProperty "parses a term" $ property $ do
      (txt, term) <- forAll $ termGen True True True True []
      parseOnly (termParser "" operators True True True True []) txt
        === Right term
  ]

typeParserTests :: TestTree
typeParserTests = testGroup "typeParser"
  [ testProperty "parses a type" $ property $ do
      (txt, typ) <- forAll $ typeGen True True True []
      parseOnly (typeParser "" True True True []) txt === Right typ
  ]

patternParserTests :: TestTree
patternParserTests = testGroup "patternParser"
  [ testCase "parses patterns with constructors, wildcards and variables" $ do
      let pat = ConstructorPattern (NameReference "Con1") "Con1"
            [ WildcardPattern
            , VariablePattern "w"
            , ConstructorPattern (NameReference "Con2") "Con2"
                                 [VariablePattern "x", VariablePattern "y"]
            , ConstructorPattern (NameReference "Con3") "Con3" []
            , VariablePattern "z"
            ]
      parseOnly (patternParser "  " True) "Con1 _ w (Con2 x y)\n  Con3 z"
        @?= Right (pat, ["z", "y", "x", "w"])
  ]

moduleNameParserTests :: TestTree
moduleNameParserTests = testGroup "moduleNameParser"
  [ testProperty "consumes capitalised words separated by dots" $ property $ do
      mname <- forAll moduleNameGen
      let txt = T.intercalate "." $ unModuleName mname
      parseOnly moduleNameParser txt === Right mname
  ]

exportsParserTests :: TestTree
exportsParserTests = testGroup "exportsParser"
  [ testCase "parses a list of exported names" $
      parseOnly exportsParser "( One, Two ,\tthree,+)"
        @?= Right ["One", "Two", "three", "+"] ]

importParserTests :: TestTree
importParserTests = testGroup "importParser"
  [ testProperty "parses an import statement" $ property $ do
      spaces <- forAll horizontalSpacesGen
      mname  <- forAll moduleNameGen
      vert   <- forAll verticalSepGen
      let txt =
            "import" <> spaces <> T.intercalate "." (unModuleName mname) <> vert
      parseOnly importParser txt === Right mname
  ]

termDefinitionTests :: TestTree
termDefinitionTests = testGroup "termDefinition"
  [ testProperty "parses a term definition" $ property $ do
      name <- forAll identifierGen
      (txt, decl) <- forAll $ termDeclarationGen name
      parseOnly termDefinition txt === Right decl
  ]

signatureTests :: TestTree
signatureTests = testGroup "signature"
  [ testProperty "parses a type signature" $ property $ do
      name <- forAll identifierGen
      (txt, sig) <- forAll $ signatureGen name
      parseOnly signature txt === Right sig
  ]

foreignImportTests :: TestTree
foreignImportTests = testGroup "foreignImport"
  [ testProperty "parses a foreign import" $ property $ do
      name <- forAll identifierGen
      (txt, fimport) <- forAll $ foreignImportGen name
      parseOnly foreignImport txt === Right fimport
  ]

typeDefinitionTests :: TestTree
typeDefinitionTests = testGroup "typeDefinition"
  [ testProperty "parses a type definition" $ property $ do
      name <- forAll constructorGen
      (txt, decl) <- forAll $ typeDeclarationGen name
      parseOnly typeDefinition txt === Right decl
  ]

variableOrReferenceTests :: TestTree
variableOrReferenceTests = testGroup "variableOrReference"
  [ testCase "parses a variable if it is in the context" $
      parseOnly (variableOrReference ["one", "two", "three"]) "two"
        @?= Right (Variable 1)

  , testCase "parses a reference if the name is not in the context" $
      parseOnly (variableOrReference []) "ref"
        @?= Right (Reference (NameReference "ref"))

  , testCase "fails if it is a reserved keyword" $
      forM_ reservedKeywords $ \keyword ->
        assertBool (T.unpack keyword)
                   (isLeft (parseOnly (variableOrReference []) keyword))
  ]

constructorReferenceTests :: TestTree
constructorReferenceTests = testGroup "constructorReference"
  [ testProperty "parses a ConstructorReference" $ property $ do
      con <- forAll constructorGen
      parseOnly constructorReference con
        === Right (ConstructorReference (NameReference con) con)
  ]

abstractionTests :: TestTree
abstractionTests = testGroup "abstraction"
  [ testProperty "parses an lambda expression" $ property $ do
      (txt, term) <- forAll $ abstractionGen []
      parseOnly (abstraction "" []) txt === Right term
  ]

ifthenelseTests :: TestTree
ifthenelseTests = testGroup "ifthenelse"
  [ testProperty "parses an if-then-else expression" $ property $ do
      (txt, term) <- forAll $ ifthenelseGen []
      parseOnly (ifthenelse "" []) txt === Right term
  ]

applicationTests :: TestTree
applicationTests = testGroup "application"
  [ testProperty "parses a term application" $ property $ do
      (txt, term) <- forAll $ applicationGen []
      parseOnly (application "" []) txt === Right term
  ]

patternMatchTests :: TestTree
patternMatchTests = testGroup "patternMatch"
  [ testProperty "parses a pattern marching" $ property $ do
      (txt, term) <- forAll $ patternMatchGen []
      parseOnly (patternMatch "" []) txt === Right term
  ]

operationTests :: TestTree
operationTests = testGroup "operation"
  [ testCase "parses operations taking precedence into account" $ do
      let txt  = " + 3 * 4 + 1"
          term = Application
                   (Application (Reference (NameReference "+"))
                                (PrimInt 1))
                   (Application
                     (Application (Reference (NameReference "+"))
                       (Application
                         (Application (Reference (NameReference "*"))
                                      (PrimInt 3))
                         (PrimInt 4)))
                     (PrimInt 1)) -- 1 + ((3 * 4) + 1)
      parseOnly (operation "" [] (PrimInt 1) operators) txt @?= Right term

  , testCase "takes parentheses into account" $ do
      let txt  = " * (2 + 3)"
          term = Application
                   (Application (Reference (NameReference "*")) (PrimInt 1))
                   (Application
                     (Application (Reference (NameReference "+")) (PrimInt 2))
                     (PrimInt 3))
      parseOnly (operation "" [] (PrimInt 1) operators) txt @?= Right term
  ]

intTermTests :: TestTree
intTermTests = testGroup "intTerm"
  [ testCase "parses a simple integer" $
      parseOnly intTerm "123" @?= Right (PrimInt 123)

  , testCase "parses a negative number" $
      parseOnly intTerm "-1" @?= Right (PrimInt (-1))
  ]

charTermTests :: TestTree
charTermTests = testGroup "charTerm"
  [ testCase "parses a single character" $
      parseOnly charTerm "'d'" @?= Right (PrimChar 'd')

  , testCase "parses an escaped character" $ do
      parseOnly charTerm "'\\\\'" @?= Right (PrimChar '\\')
      parseOnly charTerm "'\\''"  @?= Right (PrimChar '\'')
      parseOnly charTerm "'\\n'"  @?= Right (PrimChar '\n')
  ]

stringTermTests :: TestTree
stringTermTests = testGroup "stringTerm"
  [ testCase "parses a simple string" $
      parseOnly stringTerm "\"hello\"" @?= Right (PrimString "hello")

  , testCase "parses a string containing escape quotes" $
      parseOnly stringTerm "\"he\\\"llo\"" @?= Right (PrimString "he\"llo")

  , testCase "parses escaped characters inside the string" $
      parseOnly stringTerm "\"\\n\\t\"" @?= Right (PrimString "\n\t")
  ]

forallTests :: TestTree
forallTests = testGroup "forall"
  [ testProperty "parses a type beginning with forall" $ property $ do
      (txt, typ) <- forAll $ forallGen []
      parseOnly (forall "" []) txt === Right typ
  ]

functionTests :: TestTree
functionTests = testGroup "function"
  [ testProperty "parses a function type" $ property $ do
      (txt, typ) <- forAll $ functionGen []
      parseOnly (function "" []) txt === Right typ
  ]

typeApplicationTests :: TestTree
typeApplicationTests = testGroup "typeApplication"
  [ testProperty "parses a type application" $ property $ do
      (txt, typ) <- forAll $ typeApplicationGen []
      parseOnly (typeApplication "" []) txt === Right typ
  ]

identifierTests :: TestTree
identifierTests = testGroup "identifier"
  [ testProperty "consumes all characters allowed" $ property $ do
      txt <- forAll identifierGen
      parseOnly identifier txt === Right txt
  ]

constructorTests :: TestTree
constructorTests = testGroup "constructor"
  [ testProperty "consumes all characters allowed" $ property $ do
      txt <- forAll constructorGen
      parseOnly constructor txt === Right txt
  ]

operatorTests :: TestTree
operatorTests = testGroup "operator"
  [ testCase "parses an operator" $ parseOnly operator "++" @?= Right "++"

  , testCase "doesn't parse if it starts with a dot" $
      assert $ isLeft $ parseOnly operator ".+"

  , testCase "parses +:" $ parseOnly operator "+:" @?= Right "+:"
  , testCase "parses +." $ parseOnly operator "+." @?= Right "+."
  ]

commentTests :: TestTree
commentTests = testGroup "comment"
  [testProperty "consumes comments until newline" commentConsume]

commentConsume :: Property
commentConsume = property $ do
  str <- forAll $ Gen.string (Range.linear 0 100) Gen.ascii
  let parsed =
        parseOnly (comment >> endOfInput) (" \t #" <> T.pack str) == Right ()
  Hedgehog.assert $
    if '\n' `elem` str || '\r' `elem` str then not parsed else parsed

sepTests :: TestTree
sepTests = testGroup "sep"
  [ testCase "consumes all whitespaces" $
      parseOnly (sep "") " \t\r\n" @?= Right ""

  , testCase "returns the indentation level if there was a newline" $
      parseOnly (sep "") "   \n  " @?= Right "  "

  , testCase "consumes a newline only if followed by the given indentation" $ do
      parseOnly (sep "  ") " \n  " @?= Right "  "
      parseOnly (sep "  " >> string "\n a" >> endOfInput) " \n a" @?= Right ()

  , testCase "returns the given indentation if there is no newline" $ do
      parseOnly (sep "  ") "   \t " @?= Right "  "
  ]

sep1Tests :: TestTree
sep1Tests = testGroup "sep1"
  [ testCase "has the same behaviour of sep if there are \
             \whitespaces to consume" $ do
      parseOnly (sep1 "") " \t\r\n"  @?= Right ""
      parseOnly (sep1 "") "   \n  "  @?= Right "  "
      parseOnly (sep1 "  ") " \n  "  @?= Right "  "
      parseOnly (sep1 "  ") "   \t " @?= Right "  "

  , testCase "fails if there are no whitespaces to consume" $ do
      assert $ isLeft $ parseOnly (sep1 "") "a"
  ]

selectTests :: TestTree
selectTests = testGroup "select"
  [ testCase "returns a matching element with the remaining ones" $
      select (\x -> if x == 3 then Just (x+1) else Nothing) ([1,2,3,4] :: [Int])
        @?= (Just 4, [1,2,4])

  , testCase "returns Nothing with the original elements if none matches" $ do
      select (\x -> if x == 3 then Just (x+1) else Nothing) ([1,2,4] :: [Int])
        @?= (Nothing, [1,2,4])
  ]

findInContextTests :: TestTree
findInContextTests = testGroup "findInContext{L,R}"
  [ testProperty "have the same behaviour but reversed" findInContextLRProp

  , testCase "find the first matching element in its context" $
      findInContextL (>3) ([1,2,3,4,5,6] :: [Int]) @?= Just ([1,2,3], 4, [5,6])

  , testCase "return Nothing if no element matches the predicate" $
      findInContextL (>10) ([1,2,3,4,5,6,7] :: [Int]) @?= Nothing
  ]

findInContextLRProp :: Property
findInContextLRProp = property $ do
  lst <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.constant 0 100))
  x   <- forAll $ Gen.int $ Range.constant 0 100
  let transform (ls, y, rs) = (reverse rs, y, reverse ls)
  fmap transform (findInContextL (==x) lst)
    === findInContextR (==x) (reverse lst)
  fmap transform (findInContextR (==x) lst)
    === findInContextL (==x) (reverse lst)

moduleGen :: Gen (T.Text, Module)
moduleGen = do
  name <- moduleNameGen
  vsep <- verticalSepGen
  finalVSep <- Gen.element ["", vsep]

  (impTxts, imp) <- fmap unzip $ Gen.list (Range.linear 0 3) $ do
    iname <- moduleNameGen
    return ("import " <> T.intercalate "." (unModuleName iname), iname)

  typeNames <- fmap nub $ Gen.list (Range.linear 0 5) constructorGen
  termNames <- fmap nub $ Gen.list (Range.linear 0 5) identifierGen

  (typeTxts, typeDecls) <- fmap unzip $ mapM typeDeclarationGen typeNames
  let typeDefs = map (\(TypeDeclaration def) -> def) typeDecls

  (termTxts, termDefs) <- fmap unzip $ forM termNames $ \tname -> Gen.choice
    [ termDeclarationGen tname >>= \(t, TermDeclaration _ term) -> do
        (sigTxt, mSig) <- Gen.choice
          [return ("", Nothing), fmap (fmap Just) (signatureGen tname)]
        let mType = fmap (\(TypeSignature _ ty) -> ty) mSig
        return (sigTxt <> vsep <> t, TermDefinition tname term mType)
    , foreignImportGen tname >>= \(t, ForeignImport _ ty) ->
        return (t, ForeignDefinition tname ty)
    ]

  expNames <-
    if null typeNames && null termNames
      then return []
      else Gen.list (Range.linear 0 5) (Gen.element (typeNames ++ termNames))

  let (expTxt, mExp) | null expNames = ("", Nothing)
                     | otherwise = ( "(" <> T.intercalate "," expNames <> ")"
                                   , Just expNames )

      txt = "module " <> T.intercalate "." (unModuleName name)
            <> vsep <> expTxt
            <> vsep <> T.intercalate vsep impTxts
            <> vsep <> T.intercalate vsep typeTxts
            <> vsep <> T.intercalate vsep termTxts
            <> finalVSep

      _mod = Module name mExp imp typeDefs termDefs

  return (txt, _mod)

horizontalSpacesGen :: Gen T.Text
horizontalSpacesGen = Gen.text (Range.linear 1 10) (Gen.element " \t")

verticalSepGen :: Gen T.Text
verticalSepGen = do
  cmt <- choice [return "", commentGen]
  spaces <- horizontalSpacesGen
  return $ cmt <> spaces <> "\n"

commentGen :: Gen T.Text
commentGen = do
  before <- horizontalSpacesGen
  after  <- horizontalSpacesGen
  txt    <- Gen.text (Range.linear 0 100)
                     (choice [Gen.alphaNum, Gen.element " \t"])
  return $ before <> "#" <> after <> txt

termDeclarationGen :: T.Text -> Gen (T.Text, ModulePart)
termDeclarationGen name = do
  (termTxt, term) <- termGen True True True True []
  let txt = name <> " = " <> termTxt
  return (txt, TermDeclaration name term)

signatureGen :: T.Text -> Gen (T.Text, ModulePart)
signatureGen name = do
  (typTxt, typ) <- typeGen True True True []
  let txt = name <> " : " <> typTxt
  return (txt, TypeSignature name typ)

foreignImportGen :: T.Text -> Gen (T.Text, ModulePart)
foreignImportGen name = do
  (typTxt, typ) <- typeGen True True True []
  let txt = "foreign " <> name <> " : " <> typTxt
  return (txt, ForeignImport name typ)

typeDeclarationGen :: T.Text -> Gen (T.Text, ModulePart)
typeDeclarationGen name = do
  vars <- Gen.list (Range.linear 0 3) identifierGen

  (consTxts, cons) <- fmap unzip $ Gen.list (Range.linear 1 5) $ do
    cname <- constructorGen
    (paramsTxt, params) <- fmap unzip $
      Gen.list (Range.linear 0 3) (typeGen False False False (reverse vars))
    let ctxt = cname <> " " <> T.intercalate " " paramsTxt
    return (ctxt, (cname, params))

  let txt = "type " <> name <> " " <> T.intercalate " " vars <> " = "
            <> T.intercalate " | " consTxts

  return (txt, TypeDeclaration (TypeDefinition name (map (const ()) vars) cons))

-- FIXME: Parentheses
patternGen :: Bool -> Gen (T.Text, LocalPattern, [T.Text])
patternGen doCons = Gen.sized $ \size -> Gen.choice $
    [ return ("_", WildcardPattern, [])
    , fmap (\name -> (name, VariablePattern name, [name])) identifierGen
    ] ++ if size > 1 && doCons then [consPatternGen] else []

  where
    consPatternGen :: Gen (T.Text, LocalPattern, [T.Text])
    consPatternGen = do
      name <- constructorGen
      tuples <- Gen.list (Range.linear 1 3) (patternGen False)
      let txt  = foldl (\acc (t,_,_) -> acc <> " " <> t) (name <> " ") tuples
          subs = map (\(_,p,_) -> p) tuples
          pat  = ConstructorPattern (NameReference name) name subs
          vars = concat $ reverse $ map (\(_,_,v) -> v) tuples
      return (txt, pat, vars)

-- FIXME: Indentation in the term generators

termGen :: Bool -> Bool -> Bool -> Bool -> [T.Text] -> Gen (T.Text, LocalTerm)
termGen doAbs doIf doMatch doApp vars = Gen.sized $ \size -> Gen.choice $
  [ intTermGen, boolTermGen, return ("fix", FixTerm) ]
  ++ (if null vars then [] else [varGen vars])
  ++ (if size > 1 && doAbs   then [Gen.small (abstractionGen  vars)] else [])
  ++ (if size > 1 && doIf    then [Gen.small (ifthenelseGen   vars)] else [])
  ++ (if size > 1 && doApp   then [Gen.small (applicationGen  vars)] else [])
  ++ (if size > 1 && doMatch then [Gen.small (patternMatchGen vars)] else [])

abstractionGen :: [T.Text] -> Gen (T.Text, LocalTerm)
abstractionGen vars = do
  name <- identifierGen
  (termExpr, term) <- termGen True True True True (name : vars)
  let expr = "\\" <> name <> " -> " <> termExpr
  return (expr, Abstraction Nothing term) -- FIXME: Generate some with type

ifthenelseGen :: [T.Text] -> Gen (T.Text, LocalTerm)
ifthenelseGen vars = do
  (condTxt,  condTerm)  <- termGen False False False True vars
  (trueTxt,  trueTerm)  <- termGen True True True True vars
  (falseTxt, falseTerm) <- termGen True True True True vars
  let expr = "if " <> condTxt <> " then " <> trueTxt <> " else " <> falseTxt
  return (expr, IfThenElse condTerm trueTerm falseTerm)

applicationGen :: [T.Text] -> Gen (T.Text, LocalTerm)
applicationGen vars = do
  (firstTxt, firstTerm) <- termGen False False False False vars
  (txts, terms) <- fmap unzip $ Gen.list (Range.linear 1 3) $
    termGen False False False False vars
  spaces <- horizontalSpacesGen
  let txt  = foldl (\acc t -> acc <> spaces <> t) firstTxt txts
      term = foldl Application firstTerm terms
  return (txt, term)

patternMatchGen :: [T.Text] -> Gen (T.Text, LocalTerm)
patternMatchGen vars = do
  (headTxt, headTerm) <- termGen False True False True vars

  (txts, patterns) <- fmap unzip $ Gen.list (Range.linear 1 3) $ do
    (patTxt, pat, vars') <- patternGen True
    (branchTxt, term) <- termGen False False False False $ vars' ++ vars
    return (patTxt <> " -> " <> branchTxt, (pat, term))

  let initTxt = "match " <> headTxt <> " with"
      txt = foldl (\acc t -> acc <> "\n  " <> t) initTxt txts
      term = PatternMatch headTerm patterns

  return (txt, term)

intTermGen :: Gen (T.Text, LocalTerm)
intTermGen = do
  i <- Gen.int $ Range.linear 1 100
  return (T.pack (show i), PrimInt i)

boolTermGen :: Gen (T.Text, LocalTerm)
boolTermGen = do
  b <- Gen.bool
  return (if b then "true" else "false", PrimBool b)

varGen :: [T.Text] -> Gen (T.Text, LocalTerm)
varGen vars = do
  name <- Gen.element vars
  let Just i = elemIndex name vars
  return (name, Variable i)

typeGen :: Bool -> Bool -> Bool -> [T.Text] -> Gen (T.Text, LocalType)
typeGen doForall doFunc doApp vars = Gen.sized $ \size -> Gen.choice $
  [ return ("Int", PrimTypeInt), return ("Bool", PrimTypeBool), typeConsGen ]
  ++ (if null vars then [] else [typeVarGen vars])
  ++ (if size > 1 && doForall then [Gen.small (forallGen   vars)] else [])
  ++ (if size > 1 && doFunc   then [Gen.small (functionGen vars)] else [])
  ++ (if size > 1 && doApp then [Gen.small (typeApplicationGen vars)] else [])

forallGen :: [T.Text] -> Gen (T.Text, LocalType)
forallGen vars = do
  vars' <- Gen.list (Range.linear 1 3) identifierGen
  (subTxt, sub) <- typeGen True True True $ reverse vars' ++ vars
  let txt = "forall " <> T.intercalate " " vars' <> ". " <> subTxt
      mkTyp [] = id
      mkTyp (_:rest) = UniversalType . mkTyp rest
      typ = mkTyp vars' sub
  return (txt, typ)

functionGen :: [T.Text] -> Gen (T.Text, LocalType)
functionGen vars = do
  (txt1, typ1) <- typeGen False False True vars
  (txt2, typ2) <- typeGen False False True vars
  (txt3, typ3) <- typeGen False False True vars
  let txt = txt1 <> " -> " <> txt2 <> " -> " <> txt3
      typ = FunctionType typ1 (FunctionType typ2 typ3)
  return (txt, typ)

typeApplicationGen :: [T.Text] -> Gen (T.Text, LocalType)
typeApplicationGen vars = do
  (txt1, typ1) <- typeGen False False False vars
  (txt2, typ2) <- typeGen False False False vars
  (txt3, typ3) <- typeGen False False False vars
  let txt = txt1 <> " " <> txt2 <> " " <> txt3
      typ = TypeApplication (TypeApplication typ1 typ2) typ3
  return (txt, typ)

typeVarGen :: [T.Text] -> Gen (T.Text, LocalType)
typeVarGen vars = do
  name <- Gen.element vars
  let Just i = elemIndex name vars
  return (name, TypeVariable i)

typeConsGen :: Gen (T.Text, LocalType)
typeConsGen = do
  name <- constructorGen
  return (name, TypeConstructor (NameReference name))
