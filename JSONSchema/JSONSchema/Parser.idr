module JSONSchema.Parser

import Language.JSON

import Libraries.Data.SortedMap

import JSONSchema.Data
import JSONSchema.Parser.Error
import JSONSchema.JSONUtils

parseEnum : JSON -> Maybe JSONSchemaConstraints
parseEnum schema = do
    JArray options <- lookup "enum" schema
        | _ => Nothing
    pure $ JSEnum options

parseRef : JSON -> Maybe JSONSchemaConstraints
parseRef schema = do
    JString ref <- lookup "$ref" schema
        | _ => Nothing
    pure $ JSRef ref

parseAny : JSON -> Maybe JSONSchemaConstraints
parseAny (JObject xs) = Just JSAny
parseAny (JBoolean True) = Just JSAny
parseAny _ = Nothing

mutual
    parsePrimitive : JSON -> Maybe JSONSchemaConstraints
    parsePrimitive schema = parseAsType !(lookup "type" schema)
      where
        parseAsType : JSON -> Maybe JSONSchemaConstraints
        parseAsType (JString "null") = Just $ JSAtom JSNull
        parseAsType (JString "boolean") = Just $ JSAtom JSBoolean
        parseAsType (JString "object") = parseObject schema
        parseAsType (JString "array") = parseArray schema
        parseAsType (JString "number") = Just $ JSAtom JSNumber
        parseAsType (JString "string") = Just $ JSAtom JSString
        parseAsType _ = Nothing

    parseObject : JSON -> Maybe JSONSchemaConstraints
    parseObject schema = do
        props <- case lookup "properties" schema of
            Just (JObject p) => Just p
            Nothing => Just []
            _ => Nothing
        pure $ JSObject !(allOk $ map (uncurry parseProp) props)
      where
        parseProp : String -> JSON -> Maybe JSONPropertySchema
        parseProp name propSchema = pure $ MkJSONPropertySchema name !(parse propSchema)

    parseArray : JSON -> Maybe JSONSchemaConstraints
    parseArray schema = do
        itemSchema <- lookup "items" schema
        pure $ JSArray !(parse itemSchema)

    parseAnyOf : JSON -> Maybe JSONSchemaConstraints
    parseAnyOf schema = do
        JArray options <- lookup "anyOf" schema
            | _ => Nothing
        pure $ JSAnyOf !(allOk $ map parse options)

    parseConstraints : JSON -> Maybe JSONSchemaConstraints
    parseConstraints schema =
        (parseAnyOf schema) <|>
        (parseRef schema) <|>
        (parseEnum schema) <|>
        (parsePrimitive schema) <|>
        (parseAny schema)

    parseDefs : JSON -> Maybe (SortedMap String JSONSchema)
    parseDefs schema = do
        defs <- for ["$defs", "definitions"] $ \tag => case lookup tag schema of
              Just (JObject xs) => pure $ fromList !(allOk $ map (\(name, subSchema) => pure $ (name, !(parse subSchema))) $ xs)
              _ => Just empty
        pure $ foldr mergeLeft empty defs

    export
    parse : JSON -> Maybe JSONSchema
    parse json = pure $ MkJSONSchema !(parseDefs json) !(parseConstraints json)
