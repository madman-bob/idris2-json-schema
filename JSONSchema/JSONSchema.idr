module JSONSchema

import System
import System.File
import System.Path

import Language.JSON

import JSONSchema.Compiler
import public JSONSchema.Data as JSONSchema
import public JSONSchema.Names as JSONSchema
import public JSONSchema.Parser as JSONSchema

main : IO ()
main = do
    [_, file] <- getArgs
        | _ => putStrLn "Usage: json-schema input-file.json"

    Right jsonStr <- readFile file
        | Left err => printLn err

    let Just json = JSON.parse jsonStr
        | Nothing => putStrLn "Malformed JSON"

    let Just schema = JSONSchema.parse json
        | Nothing => putStrLn "Malformed JSON Schema"

    Right () <- writeFile (file <.> "idr") (concat $ map (++ "\n") $ compileSchema schema)
        | Left err => printLn err

    pure ()
