module Epic.Evaluation.Internal where

import Control.Lens
import Control.Monad.ST

import Data.List
import Data.STRef
import qualified Data.Text as T

import Epic.Language

-- | A wrapper around Term to augment it with values created with constructors
-- and Haskell functions.
data EvalTerm
  = BaseTerm Term
  | TermWithContext Term [EvalTerm]
  | Constructor Int [EvalTerm] -- ^ Parameters in reverse order
  | HaskellFunction (EvalTerm -> EvalTerm)

instance Show EvalTerm where
  show = \case
    BaseTerm t -> show t
    TermWithContext t ts ->
      "(TermWithContext (" ++ show t ++ ") ["
      ++ intercalate ", " (map (const "_") ts) ++ "])"
    Constructor i ts -> "(Constructor " ++ show i ++ " " ++ show ts ++ ")"
    HaskellFunction _ -> "(HaskellFunction _)"

instance Eq EvalTerm where
  t1 == t2 = case (t1, t2) of
    (BaseTerm t1', BaseTerm t2') -> t1' == t2'
    (Constructor i1 ts1, Constructor i2 ts2) -> i1 == i2 && ts1 == ts2
    _ -> False

toEvalTerm :: Term -> EvalTerm
toEvalTerm = BaseTerm

data Thunk s a = Thunk (ST s a) (STRef s (Maybe a))

evalThunk :: Thunk s a -> ST s a
evalThunk (Thunk f ref) = do
  mx <- readSTRef ref
  case mx of
    Just x  -> return x
    Nothing -> do
      x <- f
      writeSTRef ref (Just x)
      return x

evalWHNFCtx :: [EvalTerm] -> [(T.Text, EvalTerm -> EvalTerm)] -> EvalTerm
            -> EvalTerm
evalWHNFCtx ctx foreigns = \case
  BaseTerm (Variable i) -> case ctx ^? element i of
    Nothing -> error "variable out of bound"
    Just t  -> evalWHNFCtx ctx foreigns t

  BaseTerm (Reference (Ref _ _ (Definition (TermDefinition _ t _)))) ->
    evalWHNFCtx ctx foreigns (toEvalTerm t)

  BaseTerm (Reference (Ref _ _ (Definition (ForeignDefinition name _)))) ->
    case lookup name foreigns of
      Nothing -> error "wrong foreign reference"
      Just f  -> HaskellFunction f

  BaseTerm (ConstructorReference (Ref _ _ def) cname) ->
    case findIndex ((==cname) . fst) (def ^. constructors) of
      Nothing -> error $ "wrong constructor " ++ T.unpack cname
                         ++ " for type " ++ T.unpack (def ^. typeName)
      Just i -> Constructor i []

  BaseTerm t@(Abstraction _ _) -> TermWithContext t ctx

  BaseTerm (Application t t') ->
    case evalWHNFCtx ctx foreigns (toEvalTerm t) of
      BaseTerm (Abstraction _ t'') ->
        evalWHNFCtx (evalWHNFCtx ctx foreigns (toEvalTerm t') : ctx)
                    foreigns (toEvalTerm t'')

      TermWithContext (Abstraction _ t'') ctx' ->
        evalWHNFCtx (evalWHNFCtx ctx foreigns (toEvalTerm t') : ctx ++ ctx')
                    foreigns (toEvalTerm t'')

      BaseTerm FixTerm -> case t' of
        Abstraction _ t'' ->
          let t''' = evalWHNFCtx (t''' : ctx) foreigns (toEvalTerm t'') in t'''
        _ -> error "wrong application to fix"

      Constructor i xs -> Constructor i (toEvalTerm t' : xs)

      HaskellFunction f -> f (toEvalTerm t')

      _ -> error "wrong application"

  BaseTerm (IfThenElse tc tt tf) ->
    case evalWHNFCtx ctx foreigns (toEvalTerm tc) of
      BaseTerm (PrimBool True)  -> evalWHNFCtx ctx foreigns (toEvalTerm tt)
      BaseTerm (PrimBool False) -> evalWHNFCtx ctx foreigns (toEvalTerm tf)
      _ -> error "wrong type for if-then-else condition"

  BaseTerm (PatternMatch th patterns) ->
    let (t, extraCtx) =
           matchPatterns ctx foreigns
                         (evalWHNFCtx ctx foreigns (toEvalTerm th)) patterns
    in evalWHNFCtx (extraCtx ++ ctx) foreigns (toEvalTerm t)

  t -> t

matchPatterns :: [EvalTerm] -> [(T.Text, EvalTerm -> EvalTerm)] -> EvalTerm
              -> [(Pattern, Term)] -> (Term, [EvalTerm])
matchPatterns _ _ _ [] = error "pattern not found"
matchPatterns ctx foreigns th ((pat, tb) : patterns) =
  case matchPattern ctx foreigns th pat of
    Nothing  -> matchPatterns ctx foreigns th patterns
    Just ctx -> (tb, ctx)

matchPattern :: [EvalTerm] -> [(T.Text, EvalTerm -> EvalTerm)] -> EvalTerm
             -> Pattern -> Maybe [EvalTerm]
matchPattern ctx foreigns th = \case
  WildcardPattern -> Just []
  VariablePattern _ -> Just [th]
  ConstructorPattern (Ref _ _ def) cname subpatterns ->
    case evalWHNFCtx ctx foreigns th of
      Constructor i subterms ->
        case def ^? constructors . element i of
          Just (cname', _)
            | cname == cname' ->
                let mctxs = zipWith (matchPattern ctx foreigns)
                                    subterms (reverse subpatterns)
                in concat <$> sequence mctxs
            | otherwise -> Nothing
          Nothing -> error "wrong constructor index"
      _ -> error $ "pattern matching on non-constructor value" ++ show (th, cname, subpatterns)
