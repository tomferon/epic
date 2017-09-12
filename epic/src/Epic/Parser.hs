module Epic.Parser
  ( parseTerm
  , parseModule
  , moduleNameParser
  ) where

import Debug.Trace

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except

import           Data.Attoparsec.Text as AP
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T

import           Epic.Language

data ModulePart
  = TermDeclaration T.Text Term
  | TypeSignature T.Text Type
  | ForeignImport T.Text Type
  | TypeDeclaration TypeDefinition

parseModule :: MonadError T.Text m => T.Text -> m Module
parseModule =
  either (throwError . T.pack) return . parseOnly (moduleParser <* endOfInput)

moduleNameParser :: Parser [T.Text]
moduleNameParser = moduleNamePart `sepBy1` char '.'
  where
    moduleNamePart :: Parser T.Text
    moduleNamePart = do
      c <- peekChar'
      if isUpper c
        then takeWhile1 isAlpha
        else fail "module name should be capitalised words separated by ."

moduleParser :: Parser Module
moduleParser = do
    _ <- string "module"
    _ <- takeWhile1 isHorizontalSpace
    name <- moduleNameParser
    some horizSep

    exs <- (skipSpace *> fmap Just exports <* some horizSep) <|> return Nothing
    ims <- many imports

    parts <- (term <|> signature <|> foreignImport <|> typeDef)
             `sepBy` some horizSep
    skipSpace

    let isSignature = \case { TypeSignature _ _ -> True; _ -> False }
        isTypeDef   = \case { TypeDeclaration _ -> True; _ -> False }
        (sigs, (types, parts')) =
          fmap (partition isTypeDef) (partition isSignature parts)
    decls <- buildDecls ([], id) parts' sigs

    return $ traceShowId $
      Module name exs ims [ def | TypeDeclaration def <- types ] decls

  where
    exports :: Parser [T.Text]
    exports = do
      _ <- char '('
      skipSpace
      names <- (identifier <|> operator)
              `sepBy` (skipSpace >> char ',' >> skipSpace)
      skipSpace
      _ <- char ')'
      return names

    imports :: Parser ModuleName
    imports = do
      _ <- string "import"
      skipWhile isHorizontalSpace
      moduleNameParser <* some horizSep

    term :: Parser ModulePart
    term = do
      name <- identifier
      _ <- sep " "
      _ <- char '='
      indent <- sep " "
      term <- termParser indent operators True True True []
      return $ TermDeclaration name term

    signature :: Parser ModulePart
    signature = do
      name <- identifier
      _ <- sep " "
      _ <- char ':'
      indent <- sep " "
      typ <- traceShow (name, indent) $ typeParser indent True True True []
      traceShow typ $ return $ TypeSignature name typ

    foreignImport :: Parser ModulePart
    foreignImport = do
      _ <- string "foreign"
      _ <- sep1 " "
      name <- identifier <|> operator
      _ <- sep " "
      _ <- char ':'
      indent <- sep " "
      typ <- traceShow (name, indent) $ typeParser indent True True True []
      return $ ForeignImport name typ

    typeDef :: Parser ModulePart
    typeDef = do
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
      return $
        TypeDeclaration $ TypeDefinition name (length params) constructors

    typeCon :: T.Text -> [T.Text] -> Parser (T.Text, [Type])
    typeCon indent names = do
      name <- constructor
      indent' <- sep1 indent
      params <- (do
                  c <- peekChar'
                  if isLower c
                    then typeParser indent' False False False names
                    else typeParser indent' True True True names)
                `sepBy` sep1 indent'
      return (name, params)

    horizSep :: Parser ()
    horizSep = do
      skipWhile isHorizontalSpace
      endOfLine

    -- The first list of parts should not contain type signatures and the second
    -- should only contain type signatures.
    buildDecls :: ( [T.Text]
                  , [Definition (Maybe Type)] -> [Definition (Maybe Type)] )
               -> [ModulePart] -> [ModulePart]
               -> Parser [Definition (Maybe Type)]
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

    select :: (a -> Maybe b) -> [a] -> (Maybe b, [a])
    select f lst = case go id lst of
        Nothing -> (Nothing, lst)
        Just (x, lst') -> (Just x, lst')
      where
        go _ [] = Nothing
        go g (x : xs) = case f x of
          Just y  -> Just (y, g xs)
          Nothing -> go (g . (x :)) xs

parseTerm :: T.Text -> Either T.Text Term
parseTerm =
  either (Left . T.pack) Right
    . parseOnly (termParser "" operators True True True [] <* endOfInput)

data Operator = Operator (T.Text -> Bool) Bool

isOpChar :: Char -> Bool
isOpChar = (`elem` ("+-*/.<>=?|!@#$%^&~:;" :: String))

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
     , Operator (beginsWith '~') False
     , Operator (beginsWith ':') True
     , Operator (beginsWith '+') True
     , Operator (beginsWith '-') False
     , Operator (beginsWith '@') False
     , Operator (beginsWith '#') True
     , Operator (beginsWith '*') True
     , Operator (beginsWith '/') False
     , Operator (beginsWith '%') False
     , Operator (beginsWith '!') True
     , Operator (beginsWith '$') False
     , Operator (beginsWith '^') False
     ]

reservedKeywords :: [T.Text]
reservedKeywords = ["fix", "true", "false", "if", "then", "else"]

termParser :: T.Text -> [Operator] -> Bool -> Bool -> Bool -> [T.Text]
           -> Parser Term
termParser indent ops doAbs doIf doApp vars =
    (if doAbs then abstraction else empty)
    <|> (if doIf then ifthenelse else empty)
    <|>
    (do
      term <- (if doApp then application else empty)
        <|> (char '('
             *> termParser indent operators True True True vars
             <* char ')')
        <|> fix <|> bool <|> int <|> variableOrReference
      operation term ops <|> return term)

  where
    variableOrReference :: Parser Term
    variableOrReference = do
      name <- identifier
      guard $ name `notElem` reservedKeywords
      return $ case elemIndex name vars of
        Nothing -> Reference $ NameReference name
        Just i  -> Variable i

    abstraction :: Parser Term
    abstraction = do
      _ <- char '\\'
      indent' <- sep indent
      (name, typ) <- abstractionVarType
      _ <- sep indent'
      _ <- string "->"
      indent'' <- sep indent
      term <- termParser indent'' operators True True True $ name : vars
      return $ Abstraction typ term

    ifthenelse :: Parser Term
    ifthenelse = do
      _ <- string "if"
      indentc <- sep indent
      tc <- termParser indentc operators True True True vars
      _ <- sep indent
      _ <- string "then"
      indent1 <- sep indent
      t1 <- termParser indent1 operators True True True vars
      _ <- sep indent
      _ <- string "else"
      indent2 <- sep indent
      t2 <- termParser indent2 operators True True True vars
      return $ IfThenElse tc t1 t2

    abstractionVarType :: Parser (T.Text, Maybe Type)
    abstractionVarType =
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

    application :: Parser Term
    application = do
      t <- termParser indent [] False False False vars
      indent' <- sep1 indent
      ts <- termParser indent [] True True False vars `sepBy1` sep1 indent'
      return $ foldl Application t ts

    -- FIXME: Variables starting with fix, true or false wouldn't be parsed.

    fix :: Parser Term
    fix = string "fix" >> return Fix

    bool :: Parser Term
    bool = (string "true" >> return (PrimBool True))
           <|> (string "false" >> return (PrimBool False))

    int :: Parser Term
    int =
      ((PrimInt . read) <$> some digit)
      <|> (char '-' *> ((PrimInt . read . ('-' :)) <$> some digit))

    operation :: Term -> [Operator] -> Parser Term
    operation term ops = do
      parts <- (\f -> f []) <$> consumeOperations (Right term :)
      buildOpTree parts ops

    buildOpTree :: [Either T.Text Term] -> [Operator] -> Parser Term
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

    consumeOperations :: ([Either T.Text Term] -> [Either T.Text Term])
                      -> Parser ([Either T.Text Term] -> [Either T.Text Term])
    consumeOperations acc =
      (do
        skipSpace
        op <- operator
        skipSpace
        term <- termParser indent [] True True True vars
        consumeOperations (acc . (Left op :) . (Right term :)))
      <|> return acc

operator :: Parser T.Text
operator = do
  c <- peekChar'
  guard $ c `elem` ("|&<>=~:+-@#*/%!$^" :: String)
  takeWhile1 isOpChar

sep :: T.Text -> Parser T.Text
sep indent = do
  (do
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

typeParser :: T.Text -> Bool -> Bool -> Bool -> [T.Text] -> Parser Type
typeParser indent doForall doFunc doApp vars = do
    (if doForall then forall else empty)
    <|> (if doFunc then function else empty)
    <|> (if doApp then application else empty)
    <|> (char '(' *> typeParser indent True True True vars <* char ')')
    <|> int <|> bool <|> typeCons <|> variable

  where
    forall :: Parser Type
    forall = do
      _ <- string "forall"
      _ <- sep1 indent
      names <- identifier `sepBy1` some space
      _ <- sep indent
      _ <- char '.'
      _ <- sep1 indent
      let vars' = foldl (flip (:)) vars names
      typ <- typeParser indent True True True vars'
      return $ addUniversals names typ

    addUniversals :: [a] -> Type -> Type
    addUniversals [] t = t
    addUniversals (_ : xs) t = UniversalType (addUniversals xs t)

    function :: Parser Type
    function = do
      t <- typeParser indent False False True vars
      arrowSep
      ts <- typeParser indent True False True vars `sepBy1` arrowSep
      return $ mkFunctionType t ts

    mkFunctionType :: Type -> [Type] -> Type
    mkFunctionType t = \case
      [] -> t
      [t'] -> FunctionType t t'
      t' : ts -> FunctionType t $ mkFunctionType t' ts

    application :: Parser Type
    application = do
      t <- typeParser indent False False False vars
      indent' <- sep1 indent
      ts <- typeParser indent False False False vars `sepBy1` sep1 indent'
      return $ foldl TypeApplication t ts

    int :: Parser Type
    int = string "Int" >> return PrimTypeInt

    bool :: Parser Type
    bool = string "Bool" >> return PrimTypeBool

    variable :: Parser Type
    variable = do
      name <- identifier
      case elemIndex name vars of
        Nothing -> fail $ "undefined type variable " ++ T.unpack name
        Just i  -> return $ TypeVariable i

    typeCons :: Parser Type
    typeCons = (TypeConstructor . NameReference) <$> constructor

    arrowSep :: Parser ()
    arrowSep =
      sep indent >> string "->" >> sep indent >> return ()

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
