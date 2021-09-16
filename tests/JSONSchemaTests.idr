module JSONSchemaTests

import Test.Golden

primitiveTests : TestPool
primitiveTests = MkTestPool "Primitive" [] Nothing [
    "Array", "Boolean", "Null", "Number", "Object", "String"
  ]

composeTests : TestPool
composeTests = MkTestPool "Compose" [] Nothing [
    "Any", "Enum"
  ]

main : IO ()
main = runner [
    testPaths "Primitive" primitiveTests,
    testPaths "Compose" composeTests
  ]
  where
    testPaths : String -> TestPool -> TestPool
    testPaths dir = record { testCases $= map ((dir ++ "/") ++) }
