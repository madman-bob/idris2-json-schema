module JSONSchema.Parser

import Language.JSON

import JSONSchema.Data
import JSONSchema.JSONUtils

parsePrimitive : JSON -> Maybe JSONSchema
parsePrimitive schema = parseAsType !(lookup "type" schema)
  where
    parseAsType : JSON -> Maybe JSONSchema
    parseAsType (JString "null") = Just JSNull
    parseAsType (JString "boolean") = Just JSBoolean
    parseAsType (JString "number") = Just JSNumber
    parseAsType (JString "string") = Just JSString
    parseAsType _ = Nothing

export
parse : JSON -> Maybe JSONSchema
parse schema =
    (parsePrimitive schema)
