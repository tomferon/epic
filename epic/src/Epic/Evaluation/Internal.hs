module Epic.Evaluation.Internal where

import           Control.Lens
import           Control.Monad.Fix
import           Control.Monad.ST

import           Data.List
import           Data.STRef
import qualified Data.Text as T

import           Epic.Language

-- | A wrapper around Term to augment it with values created with constructors
-- and Haskell functions.
data EvalTerm s
  = BaseTerm Term
  | TermWithContext Term [Thunk s]
  | Constructor Int [Thunk s] -- ^ Parameters in reverse order
  | HaskellFunction (EvalTerm s -> ST s (EvalTerm s))

toEvalTerm :: Term -> EvalTerm s
toEvalTerm = BaseTerm

data Thunk s = Thunk (ST s (EvalTerm s)) (STRef s (Maybe (EvalTerm s)))

evalThunk :: Thunk s -> ST s (EvalTerm s)
evalThunk (Thunk f ref) = do
  mx <- readSTRef ref
  case mx of
    Just x  -> return x
    Nothing -> do
      x <- f
      writeSTRef ref (Just x)
      return x

makeThunk :: ST s (EvalTerm s) -> ST s (Thunk s)
makeThunk f = do
  ref <- newSTRef Nothing
  return $ Thunk f ref

evalWHNFCtx :: [Thunk s] -> [(T.Text, EvalTerm s -> ST s (EvalTerm s))]
            -> EvalTerm s -> ST s (EvalTerm s)
evalWHNFCtx ctx foreigns = \case
  BaseTerm (Variable i) -> case ctx ^? element i of
    Nothing -> error "variable out of bound"
    Just t  -> evalThunk t

  BaseTerm (Reference (Ref _ _ (Definition (TermDefinition _ t _)))) ->
    evalWHNFCtx ctx foreigns (toEvalTerm t)

  BaseTerm (Reference (Ref _ _ (Definition (ForeignDefinition name _)))) ->
    case lookup name foreigns of
      Nothing -> error "wrong foreign reference"
      Just f  -> return $ HaskellFunction f

  BaseTerm (ConstructorReference (Ref _ _ def) cname) ->
    case findIndex ((==cname) . fst) (def ^. constructors) of
      Nothing -> error $ "wrong constructor " ++ T.unpack cname
                         ++ " for type " ++ T.unpack (def ^. typeName)
      Just i -> return $ Constructor i []

  BaseTerm t@(Abstraction _ _) -> return $ TermWithContext t ctx

  BaseTerm (Application t t') -> do
    et <- evalWHNFCtx ctx foreigns $ toEvalTerm t
    case et of
      BaseTerm (Abstraction _ t'') -> do
        et' <- makeThunk $ evalWHNFCtx ctx foreigns $ toEvalTerm t'
        evalWHNFCtx (et' : ctx) foreigns (toEvalTerm t'')

      TermWithContext (Abstraction _ t'') ctx' -> do
        et' <- makeThunk $ evalWHNFCtx ctx foreigns $ toEvalTerm t'
        evalWHNFCtx (et' : ctx ++ ctx') foreigns (toEvalTerm t'')

      BaseTerm FixTerm -> case t' of
        Abstraction _ t'' -> do
          t''' <- mfix $ \thunk ->
            makeThunk $ evalWHNFCtx (thunk : ctx) foreigns (toEvalTerm t'')
          evalThunk t'''
        _ -> error "wrong application to fix"

      Constructor i xs -> do
        et' <- makeThunk $ evalWHNFCtx ctx foreigns $ toEvalTerm t'
        return $ Constructor i (et' : xs)

      HaskellFunction f -> f (toEvalTerm t')

      _ -> error "wrong application"

  BaseTerm (IfThenElse tc tt tf) -> do
    etc <- evalWHNFCtx ctx foreigns $ toEvalTerm tc
    case etc of
      BaseTerm (PrimBool True)  -> evalWHNFCtx ctx foreigns $ toEvalTerm tt
      BaseTerm (PrimBool False) -> evalWHNFCtx ctx foreigns $ toEvalTerm tf
      _ -> error "wrong type for if-then-else condition"

  BaseTerm (PatternMatch th patterns) -> do
    th' <- makeThunk $ evalWHNFCtx ctx foreigns $ toEvalTerm th
    (t, extraCtx) <- matchPatterns th' patterns
    evalWHNFCtx (extraCtx ++ ctx) foreigns (toEvalTerm t)

  t -> return t

matchPatterns :: Thunk s -> [(Pattern, Term)] -> ST s (Term, [Thunk s])
matchPatterns _ [] = error "pattern not found"
matchPatterns th ((pat, tb) : patterns) = do
  mRes <- matchPattern th pat
  case mRes of
    Nothing  -> matchPatterns th patterns
    Just ctx -> return (tb, ctx)

matchPattern :: Thunk s -> Pattern -> ST s (Maybe [Thunk s])
matchPattern th = \case
  WildcardPattern -> return $ Just []
  VariablePattern _ -> return $ Just [th]
  ConstructorPattern (Ref _ _ def) cname subpatterns -> do
    eth <- evalThunk th
    case eth of
      Constructor i subterms ->
        case def ^? constructors . element i of
          Just (cname', _)
            | cname == cname' -> do
                mctxs <- sequence $
                  zipWith matchPattern subterms (reverse subpatterns)
                return $ concat <$> sequence mctxs
            | otherwise -> return Nothing
          Nothing -> error "wrong constructor index"
      _ -> error "pattern matching on non-constructor value"
