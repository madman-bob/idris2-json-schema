module JSONSchema.Data

import Language.JSON

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
                    | JSEnum (List JSON)
                    | JSAny

    public export
    record JSONPropertySchema where
        constructor MkJSONPropertySchema
        name : String
        valueSchema : JSONSchema
