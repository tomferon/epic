{-# LANGUAGE TemplateHaskell #-}

module Epic.Base
  ( baseModules
  , baseForeigns
  , dataBoolModule
  , dataBoolForeigns
  , dataCharModule
  , dataCharForeigns
  , dataIntModule
  , dataIntForeigns
  , dataListModule
  , dataTupleModule
  , dataUnitModule
  ) where

import           Control.Monad.ST

import           Data.Char
import qualified Data.Text as T

import           Epic.Conversion
import           Epic.Evaluation
import           Epic.Language
import           Epic.TH

baseModules :: [Module]
baseModules =
  [ dataBoolModule, dataCharModule, dataIntModule, dataListModule
  , dataTupleModule, dataUnitModule
  ]

baseForeigns :: ST s [(T.Text, EvalTerm s)]
baseForeigns =
  concat <$> sequence [dataBoolForeigns, dataCharForeigns, dataIntForeigns]

dataBoolModule :: Module
dataBoolModule = $(readModule "base/Data/Bool.epic")

dataBoolForeigns :: ST s [(T.Text, EvalTerm s)]
dataBoolForeigns = do
  andFunc <- toEpic (&&)
  orFunc  <- toEpic (||)

  return
    [ ("data_bool_and", andFunc)
    , ("data_bool_or",  orFunc)
    ]

dataCharModule :: Module
dataCharModule = $(readModule "base/Data/Char.epic")

dataCharForeigns :: ST s [(T.Text, EvalTerm s)]
dataCharForeigns = do
  eqCharFunc  <- toEpic ((==) @Char)
  neqCharFunc <- toEpic ((/=) @Char)

  showCharFunc <- toEpic (T.pack . show @Char)

  isControlFunc     <- toEpic isControl
  isSpaceFunc       <- toEpic isSpace
  isLowerFunc       <- toEpic isLower
  isUpperFunc       <- toEpic isUpper
  isAlphaFunc       <- toEpic isAlpha
  isAlphaNumFunc    <- toEpic isAlphaNum
  isPrintFunc       <- toEpic isPrint
  isDigitFunc       <- toEpic isDigit
  isOctDigitFunc    <- toEpic isOctDigit
  isHexDigitFunc    <- toEpic isHexDigit
  isLetterFunc      <- toEpic isLetter
  isMarkFunc        <- toEpic isMark
  isNumberFunc      <- toEpic isNumber
  isPunctuationFunc <- toEpic isPunctuation
  isSymbolFunc      <- toEpic isSymbol
  isSeparatorFunc   <- toEpic isSeparator
  isAsciiFunc       <- toEpic isAscii
  isLatin1Func      <- toEpic isLatin1
  isAsciiUpperFunc  <- toEpic isAsciiUpper
  isAsciiLowerFunc  <- toEpic isAsciiLower

  toUpperFunc <- toEpic toUpper
  toLowerFunc <- toEpic toLower
  toTitleFunc <- toEpic toTitle

  digitToIntFunc <- toEpic digitToInt
  intToDigitFunc <- toEpic intToDigit

  ordFunc <- toEpic ord
  chrFunc <- toEpic chr

  return
    [ ("data_char_eqChar",  eqCharFunc)
    , ("data_char_neqChar", neqCharFunc)

    , ("data_char_showChar", showCharFunc)

    , ("data_char_isControl",     isControlFunc)
    , ("data_char_isSpace",       isSpaceFunc)
    , ("data_char_isLower",       isLowerFunc)
    , ("data_char_isUpper",       isUpperFunc)
    , ("data_char_isAlpha",       isAlphaFunc)
    , ("data_char_isAlphaNum",    isAlphaNumFunc)
    , ("data_char_isPrint",       isPrintFunc)
    , ("data_char_isDigit",       isDigitFunc)
    , ("data_char_isOctDigit",    isOctDigitFunc)
    , ("data_char_isHexDigit",    isHexDigitFunc)
    , ("data_char_isLetter",      isLetterFunc)
    , ("data_char_isMark",        isMarkFunc)
    , ("data_char_isNumber",      isNumberFunc)
    , ("data_char_isPunctuation", isPunctuationFunc)
    , ("data_char_isSymbol",      isSymbolFunc)
    , ("data_char_isSeparator",   isSeparatorFunc)
    , ("data_char_isAscii",       isAsciiFunc)
    , ("data_char_isLatin1",      isLatin1Func)
    , ("data_char_isAsciiUpper",  isAsciiUpperFunc)
    , ("data_char_isAsciiLower",  isAsciiLowerFunc)

    , ("data_char_toUpper", toUpperFunc)
    , ("data_char_toLower", toLowerFunc)
    , ("data_char_toTitle", toTitleFunc)

    , ("data_char_digitToInt", digitToIntFunc)
    , ("data_char_intToDigit", intToDigitFunc)

    , ("data_char_ord", ordFunc)
    , ("data_char_chr", chrFunc)
    ]

dataIntModule :: Module
dataIntModule = $(readModule "base/Data/Int.epic")

dataIntForeigns :: ST s [(T.Text, EvalTerm s)]
dataIntForeigns = do
  eqIntFunc  <- toEpic ((==) @Int)
  neqIntFunc <- toEpic ((/=) @Int)

  leIntFunc <- toEpic ((<=) @Int)
  ltIntFunc <- toEpic ((<)  @Int)
  geIntFunc <- toEpic ((>=) @Int)
  gtIntFunc <- toEpic ((>)  @Int)

  showIntFunc <- toEpic (show @Int)

  addIntFunc      <- toEpic ((+) @Int)
  subtractIntFunc <- toEpic ((-) @Int)
  multiplyIntFunc <- toEpic ((*) @Int)

  quotFunc <- toEpic (quot @Int)
  remFunc  <- toEpic (rem  @Int)
  divFunc  <- toEpic (div  @Int)
  modFunc  <- toEpic (mod  @Int)

  quotRemFunc <- toEpic (quotRem @Int)
  divModFunc  <- toEpic (divMod @Int)

  return
    [ ("data_int_eqInt", eqIntFunc)
    , ("data_int_neqInt", neqIntFunc)

    , ("data_int_leInt", leIntFunc)
    , ("data_int_ltInt", ltIntFunc)
    , ("data_int_geInt", geIntFunc)
    , ("data_int_gtInt", gtIntFunc)

    , ("data_int_showInt", showIntFunc)

    , ("data_int_addInt",      addIntFunc)
    , ("data_int_subtractInt", subtractIntFunc)
    , ("data_int_multiplyInt", multiplyIntFunc)

    , ("data_int_quot", quotFunc)
    , ("data_int_rem",  remFunc)
    , ("data_int_div",  divFunc)
    , ("data_int_mod",  modFunc)

    , ("data_int_quotRem", quotRemFunc)
    , ("data_int_divMod",  divModFunc)
    ]

dataListModule :: Module
dataListModule = $(readModule "base/Data/List.epic")

dataTupleModule :: Module
dataTupleModule = $(readModule "base/Data/Tuple.epic")

dataUnitModule :: Module
dataUnitModule = $(readModule "base/Data/Unit.epic")
