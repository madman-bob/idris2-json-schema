module JSONSchema.Compiler

import Control.Monad.Identity
import Control.Monad.Writer
import Data.SnocList

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

writeSchema : (name : String) -> JSONSchema -> SnocList String
writeSchema name (JSAtom atomSchema) = [<"\{name} : Type" , "\{name} = \{asIdrisType atomSchema}"]
writeSchema name (JSObject props) = execWriter $ do
    propNames <- for props $ \(MkJSONPropertySchema propName propSchema) => do
        let typeName = name ++ asIdrisTypeName propName
        tell $ writeSchema typeName propSchema :< ""
        pure (propName, typeName)
    tell [<"record \{name} where" , "    constructor Mk\{name}"]
    for_ propNames $ \(propName, typeName) => do
        tell [<"    \{asIdrisPropName propName} : \{typeName}"]
writeSchema name (JSArray itemSchema) = execWriter $ do
    let itemName = name ++ "Item"
    tell $ writeSchema itemName itemSchema :< ""
    tell $ [<"\{name} : Type", "\{name} = List \{itemName}"]

export
compileSchema : JSONSchema -> List String
compileSchema schema = cast {from = SnocList String} $ execWriter $ do
    tell $ writeSchema "Main" schema
