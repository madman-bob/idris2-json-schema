module JSONSchemaTests

import Test.Golden

primitiveTests : TestPool
primitiveTests = MkTestPool "Primitive" [] Nothing [
    "Boolean", "Null", "Number", "String"
  ]

main : IO ()
main = runner [
    testPaths "Primitive" primitiveTests
  ]
  where
    testPaths : String -> TestPool -> TestPool
    testPaths dir = record { testCases $= map ((dir ++ "/") ++) }
