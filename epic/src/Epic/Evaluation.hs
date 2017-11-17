module Epic.Evaluation
  ( EvalTerm(..)
  , toEvalTerm
  , evalWHNF
  ) where

import           Control.Lens

import           Data.List
import qualified Data.Text as T

import           Epic.Conversion
import           Epic.Evaluation.Internal
import           Epic.Language

-- FIXME: Replace calls to 'error' by a custom exception.

-- | Evaluate a term into its weak head normal form.
evalWHNF :: [(T.Text, EvalTerm -> EvalTerm)] -> EvalTerm -> EvalTerm
evalWHNF = evalWHNFCtx []
