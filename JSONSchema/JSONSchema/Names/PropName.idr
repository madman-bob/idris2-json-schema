module JSONSchema.Names.PropName

import JSONSchema.StringUtils

export
record PropName where
    constructor MkPropName
    asStr : String

export
Show PropName where
    show = asStr

idrisReservedNamesLower : List String
idrisReservedNamesLower = ["auto", "case", "covering", "data", "default", "do", "else", "export", "forall", "if", "implementation", "implicit", "import", "impossible", "in", "infix", "infixl", "infixr", "interface", "let", "module", "mutual", "namespace", "of", "open", "parameters", "partial", "prefix", "private", "proof", "public", "record", "rewrite", "then", "total", "using", "where", "with"]

||| Convert a JSON property identifier to a valid Idris property identifier
export
asIdrisPropName : String -> PropName
asIdrisPropName name =
    let n = filter isAlphaNum $ camelCase name in
    MkPropName $ if elem n idrisReservedNamesLower
        then n ++ "_"
        else n
