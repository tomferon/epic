import Test.Tasty

import Epic.ConversionTest
import Epic.EvaluationTest
import Epic.ParserTest
import Epic.ResolverTest
import Epic.TypeCheckerTest

main :: IO ()
main = defaultMain $ testGroup "Unit tests"
  [ conversionTests, evaluationTests, parserTests, resolverTests
  , typeCheckerTests ]
