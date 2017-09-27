module Epic.Language where

import           Control.Lens

import           Data.Eq.Deriving (deriveEq1)
import           Data.Functor.Foldable (Fix(..))
import qualified Data.Text as T

import           Text.Show.Deriving (deriveShow1)

type ModuleName = [T.Text]

data Reference
  = NameReference T.Text
--  | FQNameReference ModuleName T.Text
  deriving (Eq, Show)

data MetaF b f = MetaIndexF Int | MetaBase (b f) deriving (Eq, Show, Functor)

deriveEq1 ''MetaF
deriveShow1 ''MetaF

pattern MetaIndex i = Fix (MetaIndexF i)

data KindF f = ArrowF f f | StarF deriving (Eq, Show, Functor)

deriveEq1 ''KindF
deriveShow1 ''KindF

type Kind = Fix KindF

pattern Arrow k k' = Fix (ArrowF k k')
pattern Star = Fix StarF

type MetaKind = Fix (MetaF KindF)

pattern ArrowM k k' = Fix (MetaBase (ArrowF k k'))
pattern StarM       = Fix (MetaBase StarF)

data TypeF f
  = TypeVariableF Int
  | FunctionTypeF f f
  | UniversalTypeF f
  | PrimTypeBoolF
  | PrimTypeIntF
  | TypeConstructorF Reference
  | TypeApplicationF f f
  deriving (Eq, Show, Functor)

deriveEq1 ''TypeF
deriveShow1 ''TypeF

type Type = Fix TypeF

pattern TypeVariable i       = Fix (TypeVariableF i)
pattern FunctionType t t'    = Fix (FunctionTypeF t t')
pattern UniversalType t      = Fix (UniversalTypeF t)
pattern PrimTypeBool         = Fix PrimTypeBoolF
pattern PrimTypeInt          = Fix PrimTypeIntF
pattern TypeConstructor ref  = Fix (TypeConstructorF ref)
pattern TypeApplication t t' = Fix (TypeApplicationF t t')

type MetaType = Fix (MetaF TypeF)

pattern TypeVariableM i       = Fix (MetaBase (TypeVariableF i))
pattern FunctionTypeM t t'    = Fix (MetaBase (FunctionTypeF t t'))
pattern UniversalTypeM t      = Fix (MetaBase (UniversalTypeF t))
pattern PrimTypeBoolM         = Fix (MetaBase PrimTypeBoolF)
pattern PrimTypeIntM          = Fix (MetaBase PrimTypeIntF)
pattern TypeConstructorM ref  = Fix (MetaBase (TypeConstructorF ref))
pattern TypeApplicationM t t' = Fix (MetaBase (TypeApplicationF t t'))

data TypeDefinition k = TypeDefinition
  { _typeName     :: T.Text
  , _variables    :: [k]
  , _constructors :: [(T.Text, [Type])]
  } deriving (Eq, Show)

makeLenses ''TypeDefinition

data TermT t
  = Variable Int
  | Reference Reference
  | Abstraction (Maybe t) (TermT t)
  | Application (TermT t) (TermT t)
  | IfThenElse (TermT t) (TermT t) (TermT t)
  | PrimBool Bool
  | PrimInt Int
  | FixTerm
  deriving (Eq, Show)

type Term = TermT Type

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

data ModuleTK t k = Module
  { _moduleName  :: ModuleName
  , _exports     :: Maybe [T.Text]
  , _imports     :: [ModuleName]
  , _types       :: [TypeDefinition k]
  , _definitions :: [Definition t]
  } deriving (Eq, Show)

makeLenses ''ModuleTK

type Module = ModuleTK (Maybe Type) ()
type TypedModule = ModuleTK Type Kind
