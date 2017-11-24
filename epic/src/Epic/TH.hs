{-# LANGUAGE TemplateHaskell #-}

module Epic.TH
  ( readModule
  , epic
  ) where

import           Control.Lens
import           Control.Monad.Except (runExcept)

import           Data.String
import qualified Data.Text    as T
import qualified Data.Text.IO as T

import           Language.Haskell.TH (Q, Exp, runIO)
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax (Lift(lift))

import           Epic.Language
import           Epic.Parser

readModule :: FilePath -> Q Exp
readModule path = do
  contents <- runIO $ T.readFile path
  case runExcept (parseModule contents) of
    Left err -> fail $ T.unpack err
    Right m  -> lift m

epic :: QuasiQuoter
epic = QuasiQuoter
  { quoteExp  = epicExp
  , quotePat  = error "can't be used here"
  , quoteType = error "can't be used here"
  , quoteDec  = error "can't be used here"
  }

epicExp :: String -> Q Exp
epicExp contents = do
  case runExcept (parseModule (T.pack contents)) of
    Left err -> fail $ T.unpack err
    Right m  -> lift m
