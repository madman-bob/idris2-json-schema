module JSONSchema.Compiler

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
