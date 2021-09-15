module JSONSchemaTests

import Test.Golden

primitiveTests : TestPool
primitiveTests = MkTestPool "Primitive" [] Nothing [
    "Array", "Boolean", "Null", "Number", "Object", "String"
  ]

main : IO ()
main = runner [
    testPaths "Primitive" primitiveTests
  ]
  where
    testPaths : String -> TestPool -> TestPool
    testPaths dir = record { testCases $= map ((dir ++ "/") ++) }
