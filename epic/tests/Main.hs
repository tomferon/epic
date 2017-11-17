import Test.Tasty

import Epic.EvaluationTest
import Epic.ParserTest
import Epic.ResolverTest
import Epic.TypeCheckerTest

main :: IO ()
main = defaultMain $ testGroup "Unit tests"
  [ evaluationTests, parserTests, resolverTests, typeCheckerTests ]
