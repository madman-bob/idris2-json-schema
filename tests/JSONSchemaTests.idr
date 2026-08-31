module JSONSchemaTests

import Test.Golden

main : IO ()
main = runner [
    !(testsInDir "Support" "Support"),
    !(testsInDir "Primitive" "Primitive"),
    !(testsInDir "Compose" "Compose"),
    !(testsInDir "Refs" "Refs"),
    !(testsInDir "CLI" "CLI")
  ]
