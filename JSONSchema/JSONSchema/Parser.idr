module JSONSchema.Parser

import Language.JSON

import JSONSchema.Data
import JSONSchema.Parser.Error
import JSONSchema.JSONUtils

parseEnum : JSON -> Maybe JSONSchema
parseEnum schema = do
    JArray options <- lookup "enum" schema
        | _ => Nothing
    pure $ JSEnum options

parseAny : JSON -> Maybe JSONSchema
parseAny (JObject xs) = Just JSAny
parseAny (JBoolean True) = Just JSAny
parseAny _ = Nothing

mutual
    parsePrimitive : JSON -> Maybe JSONSchema
    parsePrimitive schema = parseAsType !(lookup "type" schema)
      where
        parseAsType : JSON -> Maybe JSONSchema
        parseAsType (JString "null") = Just $ JSAtom JSNull
        parseAsType (JString "boolean") = Just $ JSAtom JSBoolean
        parseAsType (JString "object") = parseObject schema
        parseAsType (JString "array") = parseArray schema
        parseAsType (JString "number") = Just $ JSAtom JSNumber
        parseAsType (JString "string") = Just $ JSAtom JSString
        parseAsType _ = Nothing

    parseObject : JSON -> Maybe JSONSchema
    parseObject schema = do
        props <- case lookup "properties" schema of
            Just (JObject p) => Just p
            Nothing => Just []
            _ => Nothing
        pure $ JSObject !(allOk $ map (uncurry parseProp) props)
      where
        parseProp : String -> JSON -> Maybe JSONPropertySchema
        parseProp name propSchema = pure $ MkJSONPropertySchema name !(parse propSchema)

    parseArray : JSON -> Maybe JSONSchema
    parseArray schema = do
        itemSchema <- lookup "items" schema
        pure $ JSArray !(parse itemSchema)

    parseAnyOf : JSON -> Maybe JSONSchema
    parseAnyOf schema = do
        JArray options <- lookup "anyOf" schema
            | _ => Nothing
        pure $ JSAnyOf !(allOk $ map parse options)

    export
    parse : JSON -> Maybe JSONSchema
    parse schema =
        (parseAnyOf schema) <|>
        (parseEnum schema) <|>
        (parsePrimitive schema) <|>
        (parseAny schema)
