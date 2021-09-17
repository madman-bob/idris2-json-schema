module JSONSchema.Compiler

import Data.List
import Data.List1
import Data.String

import Language.JSON

import JSONSchema.Compiler.Data
import JSONSchema.Data
import JSONSchema.StringUtils

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

mutual
    writeSchema : (name : String) -> JSONSchema -> Writer IdrisModule ()
    writeSchema name (MkJSONSchema defs constraints) = do
        for_ (SortedMap.toList defs) $ \(name, schema) => do
            writeSchema (asIdrisTypeName name) schema
            addLines [<""]
        writeSchemaConstraints name constraints

    writeSchemaConstraints : (name : String) -> JSONSchemaConstraints -> Writer IdrisModule ()
    writeSchemaConstraints name (JSObject props) = do
        propNames <- for props $ \(MkJSONPropertySchema propName propSchema) => do
            let typeName = name ++ asIdrisTypeName propName
            ref <- refSchema typeName propSchema
            pure (propName, ref)
        addLines [<"record \{name} where" , "    constructor Mk\{name}"]
        for_ propNames $ \(propName, ref) => do
            addLines [<"    \{asIdrisPropName propName} : \{ref}"]
    writeSchemaConstraints name (JSEnum options) = do
        addLines [<"data \{name} = " ++ (concat $ intersperse " | " $ map ((name ++) . jsonAsName) options)]
    writeSchemaConstraints name (JSAnyOf schemas) = do
        variants <- for (genNames name schemas) $ \(name, schema) => do
            ref <- refSchema (name ++ "T") schema
            pure $ name ++ " " ++ ref
        addLines [<"data \{name} = " ++ (concat $ intersperse " | " variants)]
    writeSchemaConstraints name schema = do
        ref <- refSchemaConstraints name schema
        addLines [<"\{name} : Type" , "\{name} = \{ref}"]

    ||| Get a potentially-anonymous reference to the type described by a schema
    ||| The type can use the given name to construct itself, if necessary
    refSchema : (name : String) -> JSONSchema -> Writer IdrisModule String
    refSchema name (MkJSONSchema defs constraints) = do
        for_ (SortedMap.toList defs) $ \(name, schema) => do
            writeSchema (asIdrisTypeName name) schema
            addLines [<""]
        refSchemaConstraints name constraints

    refSchemaConstraints : (name : String) -> JSONSchemaConstraints -> Writer IdrisModule String
    refSchemaConstraints _ (JSAtom atomSchema) = pure $ asIdrisType atomSchema
    refSchemaConstraints name (JSArray itemSchema) = do
        let itemName = name ++ "Item"
        ref <- refSchema itemName itemSchema
        pure $ "List \{ref}"
    refSchemaConstraints name (JSRef ref) = pure $ asIdrisTypeName $ last $ split (== '/') ref
    refSchemaConstraints _ JSAny = do
        addImport "Language.JSON"
        pure "JSON"
    refSchemaConstraints name schema = do
        writeSchemaConstraints name schema
        addLines [<""]
        pure name

export
compileSchema : JSONSchema -> List String
compileSchema schema = cast {from = SnocList String} $ execWriter $ do
    let idrisModule = execWriter $ writeSchema "Main" schema

    case SortedSet.toList $ imports idrisModule of
        [] => pure ()
        xs => tell $ cast (map ("import " ++) xs) :< ""

    tell $ lines idrisModule
