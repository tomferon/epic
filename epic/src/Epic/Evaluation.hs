module Epic.Evaluation
  ( EvalTerm(..)
  , toEvalTerm
  , Thunk
  , evalThunk
  , evalWHNF
  ) where

import           Control.Lens
import           Control.Monad.ST

import           Data.List
import qualified Data.Text as T

import           Epic.Evaluation.Internal
import           Epic.Language

-- FIXME: Replace calls to 'error' by a custom exception.

-- | Evaluate a term into its weak head normal form.
evalWHNF :: [(T.Text, EvalTerm s)] -> Term -> ST s (EvalTerm s)
evalWHNF foreigns = evalWHNFCtx [] foreigns . toEvalTerm
