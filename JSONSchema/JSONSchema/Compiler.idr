module JSONSchema.Compiler

import Data.List
import Data.List1
import Data.String

import Language.JSON

import JSONSchema.Compiler.Data
import JSONSchema.Data
import JSONSchema.StringUtils
import JSONSchema.TopoSort

||| Convert a JSON property identifier to a valid Idris type identifier
asIdrisTypeName : String -> String
asIdrisTypeName name = filter isAlphaNum $ title name

||| Convert a JSON property identifier to a valid Idris property identifier
asIdrisPropName : String -> String
asIdrisPropName name = camelCase $ asIdrisTypeName name

asIdrisType : JSONAtomSchema -> String
asIdrisType JSNull = "()"
asIdrisType JSBoolean = "Bool"
asIdrisType JSNumber = "Double"
asIdrisType JSString = "String"

||| Represent a JSON object as a valid Idris type identifier
jsonAsName : JSON -> String
jsonAsName JNull = "Null"
jsonAsName (JBoolean b) = show b
jsonAsName (JNumber x) = show x
jsonAsName (JString s) = asIdrisTypeName s
jsonAsName (JArray xs) = "Array" ++ (concat $ map (("_" ++) . jsonAsName) xs)
jsonAsName (JObject props) = "Object" ++ (concat $ map (\(name, val) => "_\{name}_\{jsonAsName val}") props)

||| Generate names for an anonymous collection of children of a named item
genNames : String -> List a -> List (String, a)
genNames parentName xs = map (\(n, x) => (parentName ++ show n, x)) $ enum 0 xs
  where
    enum : Nat -> List a -> List (Nat, a)
    enum n [] = []
    enum n (x :: xs) = (n, x) :: enum (S n) xs

jsonShortName : String -> String
jsonShortName = last . split (== '/')

support : JSONSchema -> SortedSet String
support (MkJSONSchema _ (JSObject props)) = foldr union empty $ map (\(MkJSONPropertySchema _ propSchema) => support propSchema) props
support (MkJSONSchema _ (JSArray itemSchema)) = support itemSchema
support (MkJSONSchema _ (JSRef ref)) = singleton $ jsonShortName ref
support (MkJSONSchema _ (JSAnyOf schemas)) = foldr union empty (map support schemas)
support (MkJSONSchema _ _) = empty

mutual
    writeSchema : (name : String) -> JSONSchema -> Writer IdrisModule ()
    writeSchema name (MkJSONSchema defs constraints) = do
        writeDefs defs
        writeSchemaConstraints name constraints

    writeSchemaConstraints : (name : String) -> JSONSchemaConstraints -> Writer IdrisModule ()
    writeSchemaConstraints name (JSObject props) = do
        propNames <- namespaceBlock name $ for props $ \(MkJSONPropertySchema propName propSchema) => do
            ref <- refSchema (asIdrisTypeName propName) propSchema
            pure (propName, ref)
        addLines [<"record \{name} where" , "    constructor Mk\{name}"]
        for_ propNames $ \(propName, ref) => do
            addLines [<"    \{asIdrisPropName propName} : \{ref}"]
    writeSchemaConstraints name (JSEnum options) = do
        addLines [<"data \{name} = " ++ (concat $ intersperse " | " $ map ((name ++) . jsonAsName) options)]
    writeSchemaConstraints name (JSAnyOf schemas) = do
        variants <- namespaceBlock name $ for (genNames name schemas) $ \(name, schema) => do
            ref <- refSchema (name ++ "T") schema {asSubexpression = True}
            pure $ name ++ " " ++ ref
        addLines [<"data \{name} = " ++ (concat $ intersperse " | " variants)]
    writeSchemaConstraints name schema = do
        ref <- refSchemaConstraints name schema
        addLines [<"\{name} : Type" , "\{name} = \{ref}"]

    ||| Get a potentially-anonymous reference to the type described by a schema
    ||| The type can use the given name to construct itself, if necessary
    refSchema : (name : String)
             -> JSONSchema
             -> {default False asSubexpression : Bool}
             -> Writer IdrisModule String
    refSchema name (MkJSONSchema defs constraints) = do
        writeDefs defs
        refSchemaConstraints name constraints {asSubexpression}

    refSchemaConstraints : (name : String)
                        -> JSONSchemaConstraints
                        -> {default False asSubexpression : Bool}
                        -> Writer IdrisModule String
    refSchemaConstraints _ (JSAtom atomSchema) = pure $ asIdrisType atomSchema
    refSchemaConstraints name (JSArray itemSchema) = do
        let itemName = name ++ "Item"
        ref <- namespaceBlock name $ refSchema itemName itemSchema {asSubexpression = True}
        if asSubexpression
            then pure $ "(List \{ref})"
            else pure $ "List \{ref}"
    refSchemaConstraints name (JSRef ref) = pure $ asIdrisTypeName $ jsonShortName ref
    refSchemaConstraints _ JSAny = do
        addImport "Language.JSON"
        pure "JSON"
    refSchemaConstraints name schema = do
        writeSchemaConstraints name schema
        addLines [<""]
        pure name

    writeDefs : SortedMap String JSONSchema -> Writer IdrisModule ()
    writeDefs defs = do
        let deps = map support defs
        for_ (topoSort deps) $ \case
            name ::: [] => writeDef name
            names => mutualBlock $ traverse_ writeDef names
      where
        writeDef : (shortName : String) -> Writer IdrisModule ()
        writeDef shortName = case lookup shortName defs of
            Nothing => pure ()
            Just schema => do
                writeSchema (asIdrisTypeName shortName) schema
                addLines [<""]

export
compileSchema : JSONSchema -> List String
compileSchema schema = cast {from = SnocList String} $ execWriter $ do
    let idrisModule = execWriter $ writeSchema "Main" schema

    case SortedSet.toList $ imports idrisModule of
        [] => pure ()
        xs => tell $ cast (map ("import " ++) xs) :< ""

    tell $ lines idrisModule
