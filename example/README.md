# Case study: number guessing game

Here, we will develop a simple project composed of a host program in Haskell
executing an guest Epic script in `example/guest/`. The host will provide the
guest with a way to access the outside world in a pure fashion.

Let's first import the modules we'll need further down and enable some language
extensions.

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import           GHC.Generics         (Generic)

import           Control.Monad.Except (runExceptT, lift, throwError)
import           Control.Monad.ST     (ST, RealWorld, stToIO)

import           Data.Maybe           (listToMaybe)
import qualified Data.Text    as T
import qualified Data.Text.IO as T

import           System.Exit          (ExitCode(..), exitFailure, exitWith)
import           System.Environment   (getArgs)
import           System.IO            ( hPutStrLn, stderr, stdout, hSetBuffering
                                      , BufferMode(..) )
import           System.Random        (randomIO)

import           Epic.Base            (baseModules, baseForeigns)
import           Epic.Conversion      (FromEpic(..))
import           Epic.Evaluation      (evalWHNF)
import           Epic.Language
import           Epic.Loader          (loadModules')
import           Epic.Resolver        (resolveModules)
import           Epic.TH              (epic, readModule)
import           Epic.TypeChecker     (typeCheckModules)
```

## Encoding of side effects

To encode side effects, we will use a structure representing a program which
will then be executed by the Haskell code. This data structure will be defined
both in Haskell and in Epic and conversion will be provided by deriving
`Generic` and `FromEpic`.

Note that we could have defined a functor and have derived a free monad. We
could have used a package such as
[free](http://hackage.haskell.org/package/free). Because, we don't want
to confuse the reader who is not aware of free monads, we won't go this way
here.

```haskell
data Program s a
  = Pure a
  | Print T.Text (Program s a)
  | GetLine (T.Text -> ST s (Program s a))
  | GetRandom (Int -> ST s (Program s a))
  deriving Generic

instance FromEpic s a => FromEpic s (Program s a)
```

There are four ways to construct a program: `Pure` is a program returning a
constant, `Print` takes the string to print and what to do next, `GetLine`
builds a program which reads a string from the input and passes it to a function
which represents what to do next and `GetRandom` is similar to `GetLine`. We use
functions in `GetLine` and `GetRandom` because the rest of the programs will
obviously depends on the result of the operation. Finally, note that there is no
instance of `FromEpic s (a -> b)` (it would be impossible to write) but there is
one for `(a -> ST s b)`, hence the use of the ST monad here. The ST monad is
used when evaluating code in Epic (STRef's are used for thunks.)

In Epic, we have the corresponding type:

```
type Program a
  = Pure a
  | Print String (Program a)
  | GetLine (String -> Program a)
  | GetRandom (Int -> Program a)
```

:warning: Be aware that the type `String` in Epic corresponds to the type
`Data.Text.Text` in Haskell. You can convert Haskell `String`s but from/to
`List Char` in Epic.

### Monadic interface

We can define `return` and `>>=` for a more Haskell-ish feel. See the full code
in [example/Common.epic](Common.epic).

```
return : forall a. a -> Program a
return = Pure

>>= : forall a b. Program a -> (a -> Program b) -> Program b
>>= = fix \bind -> \x -> \f ->
  match x with
    Pure a -> f a
    Print str y -> Print str (bind y f)
    GetLine g -> GetLine (\s -> bind (g s) f)
    GetRandom g -> GetRandom (\i -> bind (g i) f)
```

### Interpretation of a program

Now we need a function that will effectively run the program in our host
program.

```haskell
runProgram :: Program RealWorld a -> IO a
runProgram prog = case prog of
  Pure a -> return a
  Print str prog' -> T.putStr str >> runProgram prog'
  GetLine f -> do
    str <- T.getLine
    prog' <- stToIO (f str)
    runProgram prog'
  GetRandom f -> do
    i <- randomIO
    prog' <- stToIO (f i)
    runProgram prog'
```

Nothing fancy here, we just translate the constructors to the corresponding IO
code. It would also be possible to write another interpreter which would fake
the side effects for testing purposes for example.

## Organising the Epic code

All the Epic code is split in two parts: modules written by the host platform
developer and modules written by users of said platform.

In this example, `Common.epic` is distributed by the platform to the users and
`guest/Main.epic` is what a user might write.

In a real-life situation, files such as `Common.epic` should be downloadable by
the users so they can develop on their machine and use the typechecker (see
[epic-cli](../epic-cli)) before sending their code to the host. As the CLI
packages the code into a tarball, the platform should extract files into a
directory prior to calling `loadModules'`.

## Evaluation

This is the part of the code loading the code and evaluating it to the structure
we have defined.

First, we define a module `Eval` which simply contains a proxy to the guest
program which needs to be defined as `Main.main`. This prevents a user to either
omits its definition or define it with a different type. We use a quasiquoter to
define it directly in the Haskell code.

```haskell
evalMod :: Module
evalMod = [epic|module Eval
import Common
import Main
run : Program Int
run = main
|]
```

Second, we define the module `Common` which contains the Epic code of the host.
See the section above.

```haskell
commonMod :: Module
commonMod = $(readModule "Common.epic")
```

Finally, we define the evaluation function which takes the path to the directory
of the guest program and returns the evaluated term `run` converted into a
`Program Int`.

```haskell
evalProgram :: FilePath -> IO (Program RealWorld Int)
evalProgram dir = do
  eRes <- runExceptT $ do
    modules <- loadModules' (evalMod : commonMod : baseModules)
                            [dir] [ModuleName ["Main"]] -- (1)
    resolvedModules <- resolveModules modules -- (2)

    foreigns <- lift $ stToIO baseForeigns -- (3)
    typedModules <-
      typeCheckModules (Just (map fst foreigns)) resolvedModules -- (4)

    -- (5)
    let mTerm = listToMaybe
          [ term | _mod <- typedModules
                 , _moduleName _mod == ModuleName ["Eval"]
                 , Definition (TermDefinition name term _) <- _definitions _mod
                 , name == "run" ]
    term <- case mTerm of
      Nothing -> throwError "can't find term `run`"
      Just t  -> return t

    lift $ stToIO $ fromEpic =<< evalWHNF foreigns term -- (6)

  case eRes of
    Left err -> do
      hPutStrLn stderr $ "Error occured: " ++ T.unpack err
      exitFailure
    Right prog -> return prog
```

1. The directory of the guest code needs to have a module called `Main`. All
dependencies will be loaded automatically by `loadModules'`. We also preload the
`baseModules` which are built-in modules such as `Data.Int` and `Data.List`
together with `Eval` and `Common`.
2. This function resolves all references to functions or types.
3. These are the foreign definitions for the `baseModules`, i.e. the Haskell
functions which are called by the Epic code.
4. Here, we check the types of all definitions and the existence of foreign
functions.
5. It finds the resolved and typed term `run`.
6. `evalWHNF` evaluates the term to
[Weak Head Normal Form](https://wiki.haskell.org/Weak_head_normal_form) and
`fromEpic` converts the returned `EvalTerm` into our previously-defined
structure.

## Let's turn this into an executable

The main function reads the path to the Epic code from the command line and
executes it.

```haskell
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  args <- getArgs
  path <- case args of
    [p] -> return p
    _ -> do
      hPutStrLn stderr "Please specifiy the path to the guest code to interpret\
                       \ as first argument."
      exitFailure

  prog <- evalProgram path
  code <- runProgram prog
  exitWith $ if code == 0 then ExitSuccess else ExitFailure code
```
