module JSONSchema.Names.TypeName

import Data.List

import Language.JSON

import JSONSchema.StringUtils

export
record TypeName where
    constructor MkTypeName
    asStr : String

export
Show TypeName where
    show = asStr

export
Eq TypeName where
    (==) = (==) `on` asStr

export
Ord TypeName where
    compare = compare `on` asStr

idrisReservedNamesUpper : List String
idrisReservedNamesUpper = ["Delay", "Force", "Inf", "Lazy", "Type"]

||| Convert a JSON property identifier to a valid Idris type identifier
export
asIdrisTypeName : String -> TypeName
asIdrisTypeName name =
    let n = filter isAlphaNum $ title name in
    MkTypeName $ if elem n idrisReservedNamesUpper
        then n ++ "_"
        else n

export
fromString : String -> TypeName
fromString = asIdrisTypeName

compoundName : List TypeName -> TypeName
compoundName names = MkTypeName $ concat $ intersperse "_" $ map asStr names

||| Represent a JSON object as a valid Idris type identifier
export
jsonAsName : JSON -> TypeName
jsonAsName JNull = MkTypeName "Null"
jsonAsName (JBoolean b) = MkTypeName $ show b
jsonAsName (JNumber x) = MkTypeName $ show x
jsonAsName (JString s) = asIdrisTypeName s
jsonAsName (JArray xs) = compoundName $ MkTypeName "Array" :: map jsonAsName xs
jsonAsName (JObject props) = compoundName $ MkTypeName "Object" ::
    map (\(name, val) => MkTypeName $ (asStr $ asIdrisTypeName name) ++ "_" ++ (asStr $ jsonAsName val)) props

export
subName : TypeName -> TypeName -> TypeName
subName (MkTypeName name) (MkTypeName name') = MkTypeName (name ++ name')

export
record ConstructorName where
    constructor MkConstructorName
    asStr : String

export
Show ConstructorName where
    show = asStr

export
||| Name of the constructor of a type with only one constructor
constructorName : TypeName -> ConstructorName
constructorName (MkTypeName name) = MkConstructorName $ "Mk" ++ name

export
||| Names of constructors for named variants of a type
constructorNames : TypeName -> List TypeName -> List ConstructorName
constructorNames (MkTypeName name) = map $ \(MkTypeName variant) => MkConstructorName (name ++ variant)

||| Generate constructor and type names for anonymous variants of a type
export
genNames : TypeName -> List a -> List (ConstructorName, TypeName, a)
genNames (MkTypeName name) xs = map (\(n, x) => (MkConstructorName $ nth name n, MkTypeName $ nth name n ++ "T", x)) $ enum 0 xs
  where
    enum : Nat -> List a -> List (Nat, a)
    enum n [] = []
    enum n (x :: xs) = (n, x) :: enum (S n) xs

    nth : String -> Nat -> String
    nth name n = name ++ "_" ++ show n
