module JSONSchemaTests

import Test.Golden

main : IO ()
main = runner [
    !(testsInDir "Primitive" "Primitive"),
    !(testsInDir "Compose" "Compose"),
    !(testsInDir "Refs" "Refs"),
    !(testsInDir "CLI" "CLI")
  ]
