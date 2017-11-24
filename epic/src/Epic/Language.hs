{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}

module Epic.Language where

import           Control.Lens

import           Data.Eq.Deriving (deriveEq1)
import           Data.Functor.Foldable (Fix(..))
import           Data.List
import           Data.String (fromString)
import qualified Data.Text as T

import           Text.Show.Deriving (deriveShow1)

import           Language.Haskell.TH.Lib (stringE)
import           Language.Haskell.TH.Syntax (Lift(..))

instance Lift T.Text where
  lift txt = [| fromString $(stringE (T.unpack txt)) |]

newtype ModuleName
  = ModuleName { unModuleName :: [T.Text] }
  deriving (Eq, Show, Lift)

data LocalReference
  = NameReference T.Text
  | FQReference ModuleName T.Text
  deriving (Eq, Show, Lift)

data Ref a = Ref ModuleName T.Text a

instance Eq (Ref a) where
  Ref mname name _ == Ref mname' name' _ = mname == mname' && name == name'

instance Show (Ref a) where
  show (Ref mname name _) = "(Ref (" ++ show mname ++ ") " ++ show name ++ " _)"

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
  | PrimTypeCharF
  | PrimTypeStringF
  | TypeConstructorF tyref
  | TypeApplicationF f f
  deriving (Eq, Show, Lift, Functor)

deriveEq1 ''TypePF
deriveShow1 ''TypePF

instance Lift (Fix (TypePF LocalReference)) where
  lift (Fix x) = [| Fix $(lift x) |]

type TypeP tyref = Fix (TypePF tyref)

type LocalType = TypeP LocalReference

newtype FQType
  = FQType { unFQType :: TypeP (Ref (TypeDefinition FQType ())) }
  deriving (Eq, Show)

newtype Type
  = Type { unType :: TypeP (Ref (TypeDefinition Type Kind)) }
  deriving (Eq, Show)

pattern TypeVariable i       = Fix (TypeVariableF i)
pattern FunctionType t t'    = Fix (FunctionTypeF t t')
pattern UniversalType t      = Fix (UniversalTypeF t)
pattern PrimTypeBool         = Fix PrimTypeBoolF
pattern PrimTypeInt          = Fix PrimTypeIntF
pattern PrimTypeChar         = Fix PrimTypeCharF
pattern PrimTypeString       = Fix PrimTypeStringF
pattern TypeConstructor ref  = Fix (TypeConstructorF ref)
pattern TypeApplication t t' = Fix (TypeApplicationF t t')

type MetaType = Fix (MetaF (TypePF (Ref (TypeDefinition Type Kind))))

pattern TypeVariableM i       = Fix (MetaBase (TypeVariableF i))
pattern FunctionTypeM t t'    = Fix (MetaBase (FunctionTypeF t t'))
pattern UniversalTypeM t      = Fix (MetaBase (UniversalTypeF t))
pattern PrimTypeBoolM         = Fix (MetaBase PrimTypeBoolF)
pattern PrimTypeIntM          = Fix (MetaBase PrimTypeIntF)
pattern PrimTypeCharM         = Fix (MetaBase PrimTypeCharF)
pattern PrimTypeStringM       = Fix (MetaBase PrimTypeStringF)
pattern TypeConstructorM ref  = Fix (MetaBase (TypeConstructorF ref))
pattern TypeApplicationM t t' = Fix (MetaBase (TypeApplicationF t t'))

data TypeDefinition t k = TypeDefinition
  { _typeName     :: T.Text
  , _variables    :: [k]
  , _constructors :: [(T.Text, [t])]
  } deriving (Eq, Show, Lift)

makeLenses ''TypeDefinition

-- FIXME: Shouldn't it contain the index of the constructor after the resolver?
data PatternP tyref
  = ConstructorPattern tyref T.Text [PatternP tyref]
  | VariablePattern T.Text
  | WildcardPattern
  deriving (Eq, Show, Lift)

type LocalPattern = PatternP LocalReference
type FQPattern = PatternP (Ref (TypeDefinition FQType ()))
type Pattern = PatternP (Ref (TypeDefinition Type Kind))

data TermP teref tyref ty
  = Variable Int
  | Reference teref
  | ConstructorReference tyref T.Text
  | Abstraction (Maybe ty) (TermP teref tyref ty)
  | Application (TermP teref tyref ty) (TermP teref tyref ty)
  | IfThenElse (TermP teref tyref ty) (TermP teref tyref ty)
               (TermP teref tyref ty)
  | PatternMatch (TermP teref tyref ty) [(PatternP tyref, TermP teref tyref ty)]
  | PrimBool Bool
  | PrimInt Int
  | PrimChar Char
  | PrimString T.Text
  | FixTerm
  -- FIXME: | TypeAnnotation (TermP teref ty) ty
  deriving (Eq, Show, Lift)

type LocalTerm = TermP LocalReference LocalReference LocalType
type FQTerm = TermP (Ref FQDefinition) (Ref (TypeDefinition FQType ())) FQType
type Term = TermP (Ref Definition) (Ref (TypeDefinition Type Kind)) Type

data DefinitionP teref tyref ty mty
  = TermDefinition T.Text (TermP teref tyref ty) mty
  | ForeignDefinition T.Text ty
  deriving (Eq, Show, Lift)

type LocalDefinition
  = DefinitionP LocalReference LocalReference LocalType (Maybe LocalType)

newtype FQDefinition = FQDefinition
  { unFQDefinition :: DefinitionP (Ref FQDefinition)
                                  (Ref (TypeDefinition FQType ())) FQType
                                  (Maybe FQType)
  } deriving (Eq, Show)

newtype Definition = Definition
  { unDefinition :: DefinitionP (Ref Definition)
                                (Ref (TypeDefinition Type Kind)) Type Type
  } deriving (Eq, Show)

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
  } deriving (Eq, Show, Lift)

makeLenses ''ModuleP

type Module = ModuleP LocalDefinition (TypeDefinition LocalType ())
type FQModule = ModuleP FQDefinition (TypeDefinition FQType ())
type TypedModule = ModuleP Definition (TypeDefinition Type Kind)
