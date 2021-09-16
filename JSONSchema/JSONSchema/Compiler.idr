module JSONSchema.Compiler

import Data.List

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

writeSchema : (name : String) -> JSONSchema -> Writer IdrisModule ()
writeSchema name (JSAtom atomSchema) = addLines [<"\{name} : Type" , "\{name} = \{asIdrisType atomSchema}"]
writeSchema name (JSObject props) = do
    propNames <- for props $ \(MkJSONPropertySchema propName propSchema) => do
        let typeName = name ++ asIdrisTypeName propName
        writeSchema typeName propSchema
        addLines [<""]
        pure (propName, typeName)
    addLines [<"record \{name} where" , "    constructor Mk\{name}"]
    for_ propNames $ \(propName, typeName) => do
        addLines [<"    \{asIdrisPropName propName} : \{typeName}"]
writeSchema name (JSArray itemSchema) = do
    let itemName = name ++ "Item"
    writeSchema itemName itemSchema
    addLines [<"", "\{name} : Type", "\{name} = List \{itemName}"]
writeSchema name (JSEnum options) = do
    addLines [<"data \{name} = " ++ (concat $ intersperse " | " $ map ((name ++) . jsonAsName) options)]
writeSchema name JSAny = do
    addImport "Language.JSON"
    addLines [<"\{name} : Type" , "\{name} = JSON"]

export
compileSchema : JSONSchema -> List String
compileSchema schema = cast {from = SnocList String} $ execWriter $ do
    let idrisModule = execWriter $ writeSchema "Main" schema

    case SortedSet.toList $ imports idrisModule of
        [] => pure ()
        xs => tell $ cast (map ("import " ++) xs) :< ""

    tell $ lines idrisModule
