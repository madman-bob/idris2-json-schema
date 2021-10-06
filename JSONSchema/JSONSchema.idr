module JSONSchema

import System
import System.File
import System.Path

import Language.JSON

import Collie

import JSONSchema.Compiler
import public JSONSchema.Data as JSONSchema
import public JSONSchema.Names as JSONSchema
import public JSONSchema.Parser as JSONSchema

jsonSchema : Command "json-schema"
jsonSchema = MkCommand {
    description = """
    Generate Idris 2 data types from a JSON Schema
    Usage: json-schema input-file.json
    """,
    subcommands = [
        "--help" ::= basic "Print this help text" none
    ],
    modifiers = [
        "--json-casts" ::= flag "Implement Cast interfaces to JSON for the generated data types"
    ],
    arguments = filePath
  }

printUsage : IO ()
printUsage = putStrLn jsonSchema.usage

main : IO ()
main = jsonSchema.handleWith $ [\case
    MkParsedCommand _ Nothing => printUsage
    MkParsedCommand (MkRecord [jsonCasts]) (Just file) => do
        let compileOptions = MkCompileOptions jsonCasts

        Right jsonStr <- readFile file
            | Left err => printLn err

        let Just json = JSON.parse jsonStr
            | Nothing => putStrLn "Malformed JSON"

        let Just schema = JSONSchema.parse json
            | Nothing => putStrLn "Malformed JSON Schema"

        Right () <- writeFile (file <.> "idr") (concat $ map (++ "\n") $ compileSchema schema)
            | Left err => printLn err

        pure (),
    "--help" ::= [const printUsage]
  ]
