module JSONSchema.Data

import Language.JSON

import public Libraries.Data.SortedMap

public export
data JSONAtomSchema = JSNull
                    | JSBoolean
                    | JSNumber
                    | JSString

mutual
    public export
    record JSONSchema where
        constructor MkJSONSchema
        defs : SortedMap String JSONSchema
        constraints : JSONSchemaConstraints

    public export
    data JSONSchemaConstraints = JSAtom JSONAtomSchema
                               | JSObject (List JSONPropertySchema)
                               | JSArray JSONSchema
                               | JSEnum (List JSON)
                               | JSRef String
                               | JSAnyOf (List JSONSchema)
                               | JSAny

    public export
    record JSONPropertySchema where
        constructor MkJSONPropertySchema
        name : String
        valueSchema : JSONSchema
