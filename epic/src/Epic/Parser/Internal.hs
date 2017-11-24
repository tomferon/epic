module Epic.Parser.Internal where

import           Control.Applicative
import           Control.Monad

import           Data.Attoparsec.Text as AP
import           Data.Char
import           Data.List
import           Data.Monoid
import qualified Data.Text as T

import           Epic.Language

data Operator = Operator (T.Text -> Bool) Bool

operators :: [Operator]
operators =
  let beginsWith c = (== c) . flip T.index 0
  in [ Operator (== "||") True
     , Operator (== "&&") True
     , Operator (beginsWith '|') False
     , Operator (beginsWith '&') False
     , Operator (beginsWith '<') False
     , Operator (beginsWith '>') False
     , Operator (beginsWith '=') True
     , Operator (beginsWith '!') True
     , Operator (beginsWith '~') False
     , Operator (beginsWith ':') True
     , Operator (beginsWith '+') True
     , Operator (beginsWith '-') False
     , Operator (beginsWith '@') False
     , Operator (beginsWith '*') True
     , Operator (beginsWith '/') False
     , Operator (beginsWith '%') False
     , Operator (beginsWith '$') False
     , Operator (beginsWith '^') False
     ]

reservedKeywords :: [T.Text]
reservedKeywords =
  ["fix", "true", "false", "if", "then", "else", "match", "with"]

data ModulePart
  = TermDeclaration T.Text LocalTerm
  | TypeSignature T.Text LocalType
  | ForeignImport T.Text LocalType
  | TypeDeclaration (TypeDefinition LocalType ())
  deriving (Eq, Show)

-- FIXME: warning for exports that don't exist
moduleParser :: Parser Module
moduleParser = do
    _ <- string "module"
    _ <- takeWhile1 isHorizontalSpace
    name <- moduleNameParser
    some verticalSep

    exs <- (skipSpace *> fmap Just exportsParser <* some verticalSep)
           <|> return Nothing
    ims <- many importParser

    parts <- (termDefinition <|> signature <|> foreignImport <|> typeDefinition)
             `sepBy` some verticalSep
    skipSpace

    let isSignature = \case { TypeSignature _ _ -> True; _ -> False }
        isTypeDef   = \case { TypeDeclaration _ -> True; _ -> False }
        (sigs, (types, parts')) =
          fmap (partition isTypeDef) (partition isSignature parts)
    decls <- buildDecls ([], id) parts' sigs

    return $
      Module name exs ims [ def | TypeDeclaration def <- types ] decls

  where
    -- The first list of parts should not contain type signatures and the second
    -- should only contain type signatures.
    buildDecls :: ([T.Text], [LocalDefinition] -> [LocalDefinition])
               -> [ModulePart] -> [ModulePart] -> Parser [LocalDefinition]
    buildDecls (_, acc) [] [] = return (acc [])
    buildDecls _ [] _ = fail "signatures lack an accompanying binding"
    buildDecls (names, acc) (part : parts) sigs = case part of
      TermDeclaration name term
        | name `elem` names ->
            fail $ "duplicate definitions for " ++ T.unpack name
        | otherwise -> do
            let (mSig, sigs') =
                  select (\(TypeSignature name' typ) ->
                            if name == name' then Just typ else Nothing) sigs
                decl = TermDefinition name term mSig
            buildDecls (name : names, acc . (decl :)) parts sigs'
      ForeignImport name typ
        | name `elem` names ->
            fail $ "duplicate definitions for " ++ T.unpack name
        | otherwise -> do
            let decl = ForeignDefinition name typ
            buildDecls (name : names, acc . (decl :)) parts sigs
      _ -> fail "buildDecls: invariant violated"

-- FIXME: Fully-qualified references
termParser :: T.Text -> [Operator] -> Bool -> Bool -> Bool -> Bool -> [T.Text]
           -> Parser LocalTerm
termParser indent ops doAbs doIf doMatch doApp vars =
  (if doAbs then abstraction indent vars else empty)
  <|> (if doIf then ifthenelse indent vars else empty)
  <|> (if doMatch then patternMatch indent vars else empty)
  <|>
  (do
    term <-
      (if doApp then application indent vars else empty)
      <|> (char '('
           *> termParser indent operators True True True True vars
           <* char ')')
       <|> variableOrReference vars <|> constructorReference
       <|> fixTerm <|> boolTerm <|> intTerm <|> charTerm <|> stringTerm
    operation indent vars term ops <|> return term)

typeParser :: T.Text -> Bool -> Bool -> Bool -> [T.Text] -> Parser LocalType
typeParser indent doForall doFunc doApp vars = do
  (if doForall then forall indent vars else empty)
  <|> (if doFunc then function indent vars else empty)
  <|> (if doApp then typeApplication indent vars else empty)
  <|> (char '(' *> typeParser indent True True True vars <* char ')')
  <|> intType <|> boolType <|> charType <|> stringType
  <|> typeCons <|> typeVariable vars

patternParser :: T.Text -> Bool -> Parser (LocalPattern, [T.Text])
patternParser indent doConstructor = do
  (char '(' *> patternParser indent True <* char ')')
  <|> (char '_' >> return (WildcardPattern, []))
  <|> fmap (\i -> (VariablePattern i, [i])) identifier
  <|> (do
        cname <- constructor
        (patterns, extras) <-
          if doConstructor
            then
              fmap unzip $ many $ do
                indent' <- sep indent
                patternParser indent' False
            else return ([], [])
        let pat = ConstructorPattern (NameReference cname) cname patterns
        return (pat, concat (reverse extras)))

moduleNameParser :: Parser ModuleName
moduleNameParser = fmap ModuleName (moduleNamePart `sepBy1` char '.')
  where
    moduleNamePart :: Parser T.Text
    moduleNamePart = do
      c <- peekChar'
      if isUpper c
        then takeWhile1 isAlpha
        else fail "module name should be capitalised words separated by ."

{-
 - Helpers for moduleParser
 -}

exportsParser :: Parser [T.Text]
exportsParser = do
  _ <- char '('
  skipSpace
  names <- (constructor <|> identifier <|> operator)
           `sepBy` (skipSpace >> char ',' >> skipSpace)
  skipSpace
  _ <- char ')'
  return names

importParser :: Parser ModuleName
importParser = do
  _ <- string "import"
  skipWhile isHorizontalSpace
  moduleNameParser <* some verticalSep

termDefinition :: Parser ModulePart
termDefinition = do
  name <- identifier <|> operator
  _ <- sep " "
  _ <- char '='
  indent <- sep " "
  term <- termParser indent operators True True True True []
  return $ TermDeclaration name term

signature :: Parser ModulePart
signature = do
  name <- identifier <|> operator
  _ <- sep " "
  _ <- char ':'
  indent <- sep " "
  typ <- typeParser indent True True True []
  return $ TypeSignature name typ

foreignImport :: Parser ModulePart
foreignImport = do
  _ <- string "foreign"
  _ <- sep1 " "
  name <- identifier <|> operator
  _ <- sep " "
  _ <- char ':'
  indent <- sep " "
  typ <- typeParser indent True True True []
  return $ ForeignImport name typ

typeDefinition :: Parser ModulePart
typeDefinition = do
    _ <- string "type"
    indent <- sep1 " "
    name <- constructor
    indent' <- sep1 indent
    params <- identifier `sepBy` sep1 indent' -- FIXME: check uniqueness
    indent'' <- sep " "
    _ <- char '='
    _ <- sep indent''
    constructors <- typeCon indent'' (reverse params)
                    `sepBy` (sep indent'' >> char '|' >> sep indent'')
    return $ TypeDeclaration $
      TypeDefinition name (map (const ()) params) constructors

  where
    typeCon :: T.Text -> [T.Text] -> Parser (T.Text, [LocalType])
    typeCon indent names = do
      name <- constructor
      params <-
        (do
          indent' <- sep1 indent
          typeParser indent' False False False names `sepBy1` sep1 indent')
         <|> return []
      return (name, params)

verticalSep :: Parser ()
verticalSep = do
  option () comment
  skipWhile isHorizontalSpace
  endOfLine

{-
 - Helpers for termParser
 -}

variableOrReference :: [T.Text] -> Parser LocalTerm
variableOrReference vars = do
  name <- identifier
  guard $ name `notElem` reservedKeywords
  return $ case elemIndex name vars of
    Nothing -> Reference $ NameReference name
    Just i  -> Variable i

constructorReference :: Parser LocalTerm
constructorReference = do
  name <- constructor
  return $ ConstructorReference (NameReference name) name

abstraction :: T.Text -> [T.Text] -> Parser LocalTerm
abstraction indent vars = do
  _ <- char '\\'
  indent' <- sep indent
  (name, typ) <- abstractionVarType indent
  _ <- sep indent'
  _ <- string "->"
  indent'' <- sep indent
  term <- termParser indent'' operators True True True True $ name : vars
  return $ Abstraction typ term

ifthenelse :: T.Text -> [T.Text] -> Parser LocalTerm
ifthenelse indent vars = do
  _ <- string "if"
  indentc <- sep1 indent
  tc <- termParser indentc operators True True True True vars
  _ <- sep1 indent
  _ <- string "then"
  indent1 <- sep1 indent
  t1 <- termParser indent1 operators True True True True vars
  _ <- sep1 indent
  _ <- string "else"
  indent2 <- sep1 indent
  t2 <- termParser indent2 operators True True True True vars
  return $ IfThenElse tc t1 t2

abstractionVarType :: T.Text -> Parser (T.Text, Maybe LocalType)
abstractionVarType indent =
  (do name <- identifier
      return (name, Nothing))
  <|>
  (do _ <- char '('
      indent' <- sep indent
      name <- identifier
      indent'' <- sep indent'
      _ <- char ':'
      _ <- sep indent''
      typ <- typeParser indent'' True True True []
      _ <- char ')'
      return (name, Just typ))

application :: T.Text -> [T.Text] -> Parser LocalTerm
application indent vars = do
  t <- termParser indent [] False False False False vars
  indent' <- sep1 indent
  ts <- termParser indent [] True True True False vars `sepBy1` sep1 indent'
  return $ foldl Application t ts

patternMatch :: T.Text -> [T.Text] -> Parser LocalTerm
patternMatch indent vars = do
  _ <- string "match"
  indent' <- sep1 indent
  headTerm <- termParser indent' [] False True False True vars
  _ <- sep1 indent'
  _ <- string "with"
  branches <- some $ do
    indent'' <- sep1 indent
    (pat, extraVars) <- patternParser indent'' True
    _ <- sep indent''
    _ <- string "->"
    indent''' <- sep indent''
    branchTerm <- termParser (indent''' <> " ") [] True True True True
                             (extraVars ++ vars)
    return (pat, branchTerm)
  return $ PatternMatch headTerm branches

fixTerm :: Parser LocalTerm
fixTerm = string "fix" >> return FixTerm

boolTerm :: Parser LocalTerm
boolTerm = (string "true" >> return (PrimBool True))
           <|> (string "false" >> return (PrimBool False))

intTerm :: Parser LocalTerm
intTerm =
  ((PrimInt . read) <$> some digit)
  <|> (char '-' *> ((PrimInt . read . ('-' :)) <$> some digit))

charTerm :: Parser LocalTerm
charTerm = do
  _ <- char '\''
  c  <- anyChar
  c' <- if c == '\\'
          then escapedCharacter
          else return c
  _ <- char '\''
  return $ PrimChar c'

-- | Consume one character after a basckslash and return the corresponding
-- escaped character.
escapedCharacter :: Parser Char
escapedCharacter = do
  c <- anyChar
  case c of
    '\'' -> return '\''
    '"'  -> return '"'
    '\\' -> return '\\'
    'n'  -> return '\n'
    'r'  -> return '\r'
    't'  -> return '\t'
    _    -> fail $ "invalid escape character " ++ show c

stringTerm :: Parser LocalTerm
stringTerm = do
    _ <- char '"'
    str <- consumeString ""
    _ <- char '"'
    return $ PrimString str

  where
    consumeString :: T.Text -> Parser T.Text
    consumeString acc = do
      c <- peekChar'
      case c of
        '"' -> return acc
        '\\' -> do
          _ <- anyChar
          c <- escapedCharacter
          consumeString (acc <> T.singleton c)
        _ -> do
          part <- takeWhile1 (\c -> c /= '"' && c /= '\\')
          consumeString (acc <> part)

operation :: T.Text -> [T.Text] -> LocalTerm -> [Operator] -> Parser LocalTerm
operation indent vars term ops = do
    parts <- (\f -> f []) <$> consumeOperations (Right term :)
    buildOpTree parts ops

  where
    buildOpTree :: [Either T.Text LocalTerm] -> [Operator] -> Parser LocalTerm
    buildOpTree [Right t] _ = return t
    buildOpTree parts ops@(Operator f assoc : ops') = do
      let g    = either f (const False)
          mRes = (if assoc then findInContextL else findInContextR) g parts
      case mRes of
        Just (l, Left op, r) -> do
          tl <- buildOpTree l ops
          tr <- buildOpTree r ops
          return $
            Application (Application (Reference (NameReference op)) tl) tr
        _ -> buildOpTree parts ops'
    buildOpTree _ _ = fail "buildOpTree: can't build operation tree"

    consumeOperations :: ([Either T.Text LocalTerm]
                          -> [Either T.Text LocalTerm])
                      -> Parser ([Either T.Text LocalTerm]
                                 -> [Either T.Text LocalTerm])
    consumeOperations acc =
      (do
        skipSpace
        op <- operator
        skipSpace
        term <- termParser indent [] True True True True vars
        consumeOperations (acc . (Left op :) . (Right term :)))
      <|> return acc

{-
 - Helpers for typeParser
 -}

forall :: T.Text -> [T.Text] -> Parser LocalType
forall indent vars = do
    _ <- string "forall"
    _ <- sep1 indent
    names <- identifier `sepBy1` some space
    _ <- sep indent
    _ <- char '.'
    _ <- sep1 indent
    let vars' = foldl (flip (:)) vars names
    typ <- typeParser indent True True True vars'
    return $ addUniversals names typ

  where
    addUniversals :: [a] -> LocalType -> LocalType
    addUniversals [] t = t
    addUniversals (_ : xs) t = UniversalType (addUniversals xs t)

function :: T.Text -> [T.Text] -> Parser LocalType
function indent vars = do
    t <- typeParser indent False False True vars
    arrowSep
    ts <- typeParser indent True False True vars `sepBy1` arrowSep
    return $ mkFunctionType t ts

  where
    mkFunctionType :: LocalType -> [LocalType] -> LocalType
    mkFunctionType t = \case
      [] -> t
      [t'] -> FunctionType t t'
      t' : ts -> FunctionType t $ mkFunctionType t' ts

    arrowSep :: Parser ()
    arrowSep = sep indent >> string "->" >> sep indent >> return ()

typeApplication :: T.Text -> [T.Text] -> Parser LocalType
typeApplication indent vars = do
  t <- typeParser indent False False False vars
  indent' <- sep1 indent
  ts <- typeParser indent False False False vars `sepBy1` sep1 indent'
  return $ foldl TypeApplication t ts

intType :: Parser LocalType
intType = string "Int" >> return PrimTypeInt

boolType :: Parser LocalType
boolType = string "Bool" >> return PrimTypeBool

charType :: Parser LocalType
charType = string "Char" >> return PrimTypeChar

stringType :: Parser LocalType
stringType = string "String" >> return PrimTypeString

typeVariable :: [T.Text] -> Parser LocalType
typeVariable vars = do
  name <- identifier
  case elemIndex name vars of
    Nothing -> fail $ "undefined type variable " ++ T.unpack name
    Just i  -> return $ TypeVariable i

typeCons :: Parser LocalType
typeCons = (TypeConstructor . NameReference) <$> constructor

{-
 - Generic helpers
 -}

identifier :: Parser T.Text
identifier = do
  c <- peekChar'
  if isLower c
    then takeWhile1 $ \c' -> isAlphaNum c' || c' `elem` ("'_" :: String)
    else fail $ "invalid character at beginning of identifier " ++ show c

constructor :: Parser T.Text
constructor = do
  c <- peekChar'
  if isUpper c
    then takeWhile1 $ \c' -> isAlphaNum c' || c' `elem` ("'_" :: String)
    else fail $ "invalid character at beginning of constructor " ++ show c

isOpChar :: Char -> Bool
isOpChar = (`elem` ("+-*/.<>=?|!@$%^&~:;" :: String))

operator :: Parser T.Text
operator = do
  c <- peekChar'
  guard $ c `elem` ("+-*/<>=?|!@$%^&~;:" :: String)
  op <- takeWhile1 isOpChar
  guard $ op /= "->" && op /= ":"
  return op

comment :: Parser ()
comment = do
  skipWhile isHorizontalSpace
  _ <- char '#'
  skipWhile (not . isEndOfLine)

sep :: T.Text -> Parser T.Text
sep indent = do
  (do
    _ <- comment `sepBy` endOfLine
    skipWhile isHorizontalSpace
    endOfLine
    string indent
    sp <- AP.takeWhile isHorizontalSpace
    sep $ indent <> sp)
   <|>
   (skipWhile isHorizontalSpace >> return indent)

sep1 :: T.Text -> Parser T.Text
sep1 indent = do
  c <- peekChar'
  guard $ isSpace c
  sep indent

select :: (a -> Maybe b) -> [a] -> (Maybe b, [a])
select f lst = case go id lst of
    Nothing -> (Nothing, lst)
    Just (x, lst') -> (Just x, lst')
  where
    go _ [] = Nothing
    go g (x : xs) = case f x of
      Just y  -> Just (y, g xs)
      Nothing -> go (g . (x :)) xs

findInContextL :: (a -> Bool) -> [a] -> Maybe ([a], a, [a])
findInContextL f = go id
  where
    go _ [] = Nothing
    go build (x : xs) | f x = Just (build [], x, xs)
                      | otherwise = go (build . (x :)) xs

findInContextR :: (a -> Bool) -> [a] -> Maybe ([a], a, [a])
findInContextR f = go (id, Nothing)
  where
    go (_, ans) [] = ans
    go (build, ans) (x : xs)
      | f x = go (build . (x :), Just (build [], x, xs)) xs
      | otherwise = go (build . (x :), ans) xs
