{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TemplateHaskell #-}

module Epic.Language where

import           Control.Lens

import qualified Data.Text as T

type ModuleName = [T.Text]

data Reference
  = NameReference T.Text
--  | FQNameReference ModuleName T.Text
  deriving (Eq, Show)

data TypeI i
  = TypeVariable i
  | FunctionType (TypeI i) (TypeI i)
  | UniversalType (TypeI i)
  | PrimTypeBool
  | PrimTypeInt
  | TypeConstructor Reference
  | TypeApplication (TypeI i) (TypeI i)
  deriving (Eq, Show, Foldable)

type Type = TypeI Int

data TypeDefinition = TypeDefinition
  { _typeName     :: T.Text
  , _variables    :: Int
  , _constructors :: [(T.Text, [Type])]
  } deriving (Eq, Show)

makeLenses ''TypeDefinition

data TermI i
  = Variable Int
  | Reference Reference
  | Abstraction (Maybe (TypeI i)) (TermI i)
  | Application (TermI i) (TermI i)
  | IfThenElse (TermI i) (TermI i) (TermI i)
  | PrimBool Bool
  | PrimInt Int
  | Fix
  deriving (Eq, Show)

type Term = TermI Int

data Definition t
  = TermDefinition T.Text Term t
  | ForeignDefinition T.Text Type
  deriving (Eq, Show)

defName :: Lens' (Definition t) T.Text
defName f = \case
  TermDefinition n te ty -> fmap (\n' -> TermDefinition n' te ty) (f n)
  ForeignDefinition n ty -> fmap (\n' -> ForeignDefinition n' ty) (f n)

defType :: Lens' (Definition Type) Type
defType f = \case
  TermDefinition n te ty -> fmap (TermDefinition n te) (f ty)
  ForeignDefinition n ty -> fmap (ForeignDefinition n) (f ty)

data ModuleT t = Module
  { _moduleName  :: ModuleName
  , _exports     :: Maybe [T.Text]
  , _imports     :: [ModuleName]
  , _types       :: [TypeDefinition]
  , _definitions :: [Definition t]
  } deriving (Eq, Show)

makeLenses ''ModuleT

type Module = ModuleT (Maybe Type)
type TypedModule = ModuleT Type
