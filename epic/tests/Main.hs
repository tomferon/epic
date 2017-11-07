import Test.Tasty

import Epic.ParserTest
import Epic.ResolverTest
import Epic.TypeCheckerTest

main :: IO ()
main = defaultMain $ testGroup "main"
  [ parserTests, resolverTests, typeCheckerTests ]
