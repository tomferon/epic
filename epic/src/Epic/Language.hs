{-# LANGUAGE RankNTypes #-}

module Epic.Language where

import           Control.Lens

import           Data.Eq.Deriving (deriveEq1)
import           Data.Functor.Foldable (Fix(..))
import           Data.List
import qualified Data.Text as T

import           Text.Show.Deriving (deriveShow1)

newtype ModuleName = ModuleName { unModuleName :: [T.Text] } deriving (Eq, Show)

data LocalReference
  = NameReference T.Text
  | FQReference ModuleName T.Text
  deriving (Eq, Show)

data Ref a = Ref ModuleName T.Text a deriving (Eq, Show)

data MetaF b f = MetaIndexF Int | MetaBase (b f) deriving (Eq, Show, Functor)

deriveEq1 ''MetaF
deriveShow1 ''MetaF

pattern MetaIndex i = Fix (MetaIndexF i)

data KindF f = ArrowF f f | StarF deriving (Eq, Show, Functor)

deriveEq1 ''KindF
deriveShow1 ''KindF

type Kind = Fix KindF

pattern Arrow k k' = Fix (ArrowF k k')
pattern Star       = Fix StarF

type MetaKind = Fix (MetaF KindF)

pattern ArrowM k k' = Fix (MetaBase (ArrowF k k'))
pattern StarM       = Fix (MetaBase StarF)

data TypePF tyref f
  = TypeVariableF Int
  | FunctionTypeF f f
  | UniversalTypeF f
  | PrimTypeBoolF
  | PrimTypeIntF
  | TypeConstructorF tyref
  | TypeApplicationF f f
  deriving (Eq, Show, Functor)

deriveEq1 ''TypePF
deriveShow1 ''TypePF

type TypeP tyref = Fix (TypePF tyref)
type LocalType = TypeP LocalReference
newtype FQType = FQType { unFQType :: TypeP (Ref (TypeDefinition FQType ())) }
newtype Type
  = Type { unType :: TypeP (Ref (TypeDefinition Type Kind)) }
  deriving (Eq, Show)

pattern TypeVariable i       = Fix (TypeVariableF i)
pattern FunctionType t t'    = Fix (FunctionTypeF t t')
pattern UniversalType t      = Fix (UniversalTypeF t)
pattern PrimTypeBool         = Fix PrimTypeBoolF
pattern PrimTypeInt          = Fix PrimTypeIntF
pattern TypeConstructor ref  = Fix (TypeConstructorF ref)
pattern TypeApplication t t' = Fix (TypeApplicationF t t')

type MetaType = Fix (MetaF (TypePF (Ref (TypeDefinition Type Kind))))

pattern TypeVariableM i       = Fix (MetaBase (TypeVariableF i))
pattern FunctionTypeM t t'    = Fix (MetaBase (FunctionTypeF t t'))
pattern UniversalTypeM t      = Fix (MetaBase (UniversalTypeF t))
pattern PrimTypeBoolM         = Fix (MetaBase PrimTypeBoolF)
pattern PrimTypeIntM          = Fix (MetaBase PrimTypeIntF)
pattern TypeConstructorM ref  = Fix (MetaBase (TypeConstructorF ref))
pattern TypeApplicationM t t' = Fix (MetaBase (TypeApplicationF t t'))

data TypeDefinition t k = TypeDefinition
  { _typeName     :: T.Text
  , _variables    :: [k]
  , _constructors :: [(T.Text, [t])]
  } deriving (Eq, Show)

makeLenses ''TypeDefinition

data TermP teref tyref ty
  = Variable Int
  | Reference teref
  | ConstructorReference tyref
  | Abstraction (Maybe ty) (TermP teref tyref ty)
  | Application (TermP teref tyref ty) (TermP teref tyref ty)
  | IfThenElse (TermP teref tyref ty) (TermP teref tyref ty)
               (TermP teref tyref ty)
  | PrimBool Bool
  | PrimInt Int
  | FixTerm
  -- FIXME: | TypeAnnotation (TermP teref ty) ty
  -- The following is impossible to construct through parsing.
  | Constructor Int [TermP teref tyref ty] ty
  deriving (Eq, Show)

type LocalTerm = TermP LocalReference LocalReference LocalType
type FQTerm = TermP (Ref FQDefinition) (Ref (TypeDefinition FQType ())) FQType
type Term = TermP (Ref Definition) (Ref (TypeDefinition Type Kind)) Type

data DefinitionP teref tyref ty mty
  = TermDefinition T.Text (TermP teref tyref ty) mty
  | ForeignDefinition T.Text ty
  deriving (Eq, Show)

type LocalDefinition
  = DefinitionP LocalReference LocalReference LocalType (Maybe LocalType)

newtype FQDefinition = FQDefinition
  { unFQDefinition :: DefinitionP (Ref FQDefinition)
                                  (Ref (TypeDefinition FQType ())) FQType
                                  (Maybe FQType) }

newtype Definition = Definition
  { unDefinition :: DefinitionP (Ref Definition)
                                (Ref (TypeDefinition Type Kind)) Type Type }

defName :: Lens' (DefinitionP teref tyref ty mty) T.Text
defName f = \case
  TermDefinition n te ty -> fmap (\n' -> TermDefinition n' te ty) (f n)
  ForeignDefinition n ty -> fmap (\n' -> ForeignDefinition n' ty) (f n)

defType :: Lens' (DefinitionP teref tyref ty ty) ty
defType f = \case
  TermDefinition n te ty -> fmap (TermDefinition n te) (f ty)
  ForeignDefinition n ty -> fmap (ForeignDefinition n) (f ty)

data ModuleP tedef tydef = Module
  { _moduleName  :: ModuleName
  , _exports     :: Maybe [T.Text]
  , _imports     :: [ModuleName]
  , _types       :: [tydef]
  , _definitions :: [tedef]
  } deriving (Eq, Show)

makeLenses ''ModuleP

type Module = ModuleP LocalDefinition (TypeDefinition LocalType ())
type FQModule = ModuleP FQDefinition (TypeDefinition FQType ())
type TypedModule = ModuleP Definition (TypeDefinition Type Kind)
