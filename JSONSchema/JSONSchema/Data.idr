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
        valueSchema : JSONSchema ref
