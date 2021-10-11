module JSONSchema.Parser

import Data.List1
import Data.String

import Language.JSON

import Libraries.Data.SortedMap

import JSONSchema.Data
import JSONSchema.Names
import JSONSchema.Parser.Error
import JSONSchema.JSONUtils

parseEnum : JSON -> Maybe (JSONSchemaConstraints QTypeName)
parseEnum schema = do
    JArray options <- lookup "enum" schema
        | _ => Nothing
    pure $ JSEnum options

parseRef : (opts : CompileOptions) => JSON -> Maybe (JSONSchemaConstraints QTypeName)
parseRef schema = do
    JString ref <- lookup "$ref" schema
        | _ => Nothing
    let "#" ::: [_, name] = split (== '/') ref
        | _ => Nothing
    pure $ JSRef $ opts.moduleName <.> asIdrisTypeName name

parseAny : JSON -> Maybe (JSONSchemaConstraints QTypeName)
parseAny (JObject xs) = Just JSAny
parseAny (JBoolean True) = Just JSAny
parseAny _ = Nothing

parseRequiredFields : JSON -> Maybe (List String)
parseRequiredFields (JArray xs) = allOk $ map parseString xs
  where
    parseString : JSON -> Maybe String
    parseString (JString str) = Just str
    parseString _ = Nothing
parseRequiredFields _ = Nothing

mutual
    parsePrimitive : CompileOptions => JSON -> Maybe (JSONSchemaConstraints QTypeName)
    parsePrimitive schema = parseAsType !(lookup "type" schema)
      where
        parseAsType : JSON -> Maybe (JSONSchemaConstraints QTypeName)
        parseAsType (JString "null") = Just $ JSAtom JSNull
        parseAsType (JString "boolean") = Just $ JSAtom JSBoolean
        parseAsType (JString "object") = parseObject schema
        parseAsType (JString "array") = parseArray schema
        parseAsType (JString "number") = Just $ JSAtom JSNumber
        parseAsType (JString "string") = Just $ JSAtom JSString
        parseAsType _ = Nothing

    parseObject : CompileOptions => JSON -> Maybe (JSONSchemaConstraints QTypeName)
    parseObject schema = do
        props <- case lookup "properties" schema of
            Just (JObject p) => Just p
            Nothing => Just []
            _ => Nothing
        requiredFields <- case lookup "required" schema of
            Just r => parseRequiredFields r
            Nothing => Just []
        pure $ JSObject !(allOk $ map (uncurry $ parseProp requiredFields) props)
      where
        parseProp : List String -> String -> JSON -> Maybe (JSONPropertySchema QTypeName)
        parseProp requiredFields name propSchema = pure $ MkJSONPropertySchema name (elem name requiredFields) !(parse propSchema)

    parseArray : CompileOptions => JSON -> Maybe (JSONSchemaConstraints QTypeName)
    parseArray schema = do
        itemSchema <- lookup "items" schema
        pure $ JSArray !(parse itemSchema)

    parseAnyOf : CompileOptions => JSON -> Maybe (JSONSchemaConstraints QTypeName)
    parseAnyOf schema = do
        JArray options <- lookup "anyOf" schema
            | _ => Nothing
        pure $ JSAnyOf !(allOk $ map parse options)

    parseConstraints : CompileOptions => JSON -> Maybe (JSONSchemaConstraints QTypeName)
    parseConstraints schema =
        (parseAnyOf schema) <|>
        (parseRef schema) <|>
        (parseEnum schema) <|>
        (parsePrimitive schema) <|>
        (parseAny schema)

    parseDefs : (opts : CompileOptions) => JSON -> Maybe (SortedMap QTypeName (JSONSchema QTypeName))
    parseDefs schema = do
        defs <- for ["$defs", "definitions"] $ \tag => case lookup tag schema of
              Just (JObject xs) => pure $ fromList !(allOk $ map (\(name, subSchema) => pure $ (opts.moduleName <.> asIdrisTypeName name, !(parse subSchema))) $ xs)
              _ => Just empty
        pure $ foldr mergeLeft empty defs

    export
    parse : CompileOptions => JSON -> Maybe (JSONSchema QTypeName)
    parse json = pure $ MkJSONSchema !(parseDefs json) !(parseConstraints json)
