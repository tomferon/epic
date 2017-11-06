import Test.Tasty

import Epic.ParserTest

main :: IO ()
main = defaultMain $ testGroup "main"
  [ parserTests ]
