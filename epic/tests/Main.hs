import Test.Tasty

import Epic.BaseTest
import Epic.ConversionTest
import Epic.EvaluationTest
import Epic.ParserTest
import Epic.ResolverTest
import Epic.TypeCheckerTest

main :: IO ()
main = do
  baseTests <- mkBaseTests
  defaultMain $ testGroup "Unit tests"
    [ baseTests, conversionTests, evaluationTests, parserTests, resolverTests
    , typeCheckerTests ]
