module JSONSchema.Data

import Language.JSON

import public Libraries.Data.SortedMap

import JSONSchema.Names

public export
record CompileOptions where
    constructor MkCompileOptions
    moduleName : QTypeName
    schemaName : TypeName
    jsonCasts : Bool

public export
data JSONAtomSchema = JSNull
                    | JSBoolean
                    | JSNumber
                    | JSString

mutual
    public export
    record JSONSchema (ref : Type) where
        constructor MkJSONSchema
        defs : SortedMap ref (JSONSchema ref)
        constraints : JSONSchemaConstraints ref

    public export
    data JSONSchemaConstraints ref = JSAtom JSONAtomSchema
                                   | JSObject (List (JSONPropertySchema ref))
                                   | JSArray (JSONSchema ref)
                                   | JSEnum (List JSON)
                                   | JSRef ref
                                   | JSAnyOf (List (JSONSchema ref))
                                   | JSAny

    public export
    record JSONPropertySchema (ref : Type) where
        constructor MkJSONPropertySchema
        name : String
        required : Bool
        valueSchema : JSONSchema ref

export
simpleSchema : Ord ref => JSONSchemaConstraints ref -> JSONSchema ref
simpleSchema constraints = MkJSONSchema empty constraints
