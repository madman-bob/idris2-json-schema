module JSONSchemaTests

import Test.Golden

primitiveTests : TestPool
primitiveTests = MkTestPool "Primitive" [] Nothing [
    "Array", "Boolean", "Nested", "Null", "Number", "Object", "String"
  ]

composeTests : TestPool
composeTests = MkTestPool "Compose" [] Nothing [
    "Any", "AnyOf", "Brackets", "Enum"
  ]

refTests : TestPool
refTests = MkTestPool "Ref" [] Nothing [
    "Definitions"
  ]

main : IO ()
main = runner [
    testPaths "Primitive" primitiveTests,
    testPaths "Compose" composeTests,
    testPaths "Refs" refTests
  ]
  where
    testPaths : String -> TestPool -> TestPool
    testPaths dir = record { testCases $= map ((dir ++ "/") ++) }
