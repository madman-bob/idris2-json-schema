module JSONSchema.Data

public export
data JSONAtomSchema = JSNull
                    | JSBoolean
                    | JSNumber
                    | JSString

mutual
    public export
    data JSONSchema = JSAtom JSONAtomSchema
                    | JSObject (List JSONPropertySchema)
                    | JSArray JSONSchema
                    | JSAny

    public export
    record JSONPropertySchema where
        constructor MkJSONPropertySchema
        name : String
        valueSchema : JSONSchema
