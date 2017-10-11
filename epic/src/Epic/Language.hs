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
  | FQReference FQRef
  deriving (Eq, Show)

data FQRef = FQRef ModuleName T.Text deriving (Eq, Show)

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

data TypeRF r f
  = TypeVariableF Int
  | FunctionTypeF f f
  | UniversalTypeF f
  | PrimTypeBoolF
  | PrimTypeIntF
  | TypeConstructorF r
  | TypeApplicationF f f
  deriving (Eq, Show, Functor)

deriveEq1 ''TypeRF
deriveShow1 ''TypeRF

type TypeR r = Fix (TypeRF r)
type LocalType = TypeR LocalReference
type Type = TypeR FQRef

pattern TypeVariable i       = Fix (TypeVariableF i)
pattern FunctionType t t'    = Fix (FunctionTypeF t t')
pattern UniversalType t      = Fix (UniversalTypeF t)
pattern PrimTypeBool         = Fix PrimTypeBoolF
pattern PrimTypeInt          = Fix PrimTypeIntF
pattern TypeConstructor ref  = Fix (TypeConstructorF ref)
pattern TypeApplication t t' = Fix (TypeApplicationF t t')

type MetaType = Fix (MetaF (TypeRF FQRef))

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

data TermRT r t
  = Variable Int
  | Reference r
  | Abstraction (Maybe t) (TermRT r t)
  | Application (TermRT r t) (TermRT r t)
  | IfThenElse (TermRT r t) (TermRT r t) (TermRT r t)
  | PrimBool Bool
  | PrimInt Int
  | FixTerm
  deriving (Eq, Show)

type LocalTerm = TermRT LocalReference LocalType
type Term = TermRT FQRef Type

data Definition r t
  = TermDefinition T.Text (TermRT r (TypeR r)) t
  | ForeignDefinition T.Text (TypeR r)
  deriving (Eq, Show)

defName :: Lens' (Definition r t) T.Text
defName f = \case
  TermDefinition n te ty -> fmap (\n' -> TermDefinition n' te ty) (f n)
  ForeignDefinition n ty -> fmap (\n' -> ForeignDefinition n' ty) (f n)

defType :: Lens' (Definition r (TypeR r)) (TypeR r)
defType f = \case
  TermDefinition n te ty -> fmap (TermDefinition n te) (f ty)
  ForeignDefinition n ty -> fmap (ForeignDefinition n) (f ty)

data ModuleRTK r t k = Module
  { _moduleName  :: ModuleName
  , _exports     :: Maybe [T.Text]
  , _imports     :: [ModuleName]
  , _types       :: [TypeDefinition (TypeR r) k]
  , _definitions :: [Definition r t]
  } deriving (Eq, Show)

makeLenses ''ModuleRTK

type Module = ModuleRTK LocalReference (Maybe LocalType) ()
type FQModule = ModuleRTK FQRef (Maybe Type) ()
type TypedModule = ModuleRTK FQRef Type Kind
