module JSONSchema.Compiler

import Data.List
import Data.List1
import Data.String

import Language.JSON

import JSONSchema.Compiler.Data
import JSONSchema.Data
import JSONSchema.Names
import JSONSchema.TopoSort

asIdrisType : JSONAtomSchema -> String
asIdrisType JSNull = "()"
asIdrisType JSBoolean = "Bool"
asIdrisType JSNumber = "Double"
asIdrisType JSString = "String"

support : JSONSchema QTypeName -> SortedSet QTypeName
support (MkJSONSchema _ (JSObject props)) = foldr union empty $ map (\(MkJSONPropertySchema _ propSchema) => support propSchema) props
support (MkJSONSchema _ (JSArray itemSchema)) = support itemSchema
support (MkJSONSchema _ (JSRef ref)) = singleton ref
support (MkJSONSchema _ (JSAnyOf schemas)) = foldr union empty (map support schemas)
support (MkJSONSchema _ _) = empty

mutual
    writeSchema : QTypeName -> JSONSchema QTypeName -> Writer IdrisModule ()
    writeSchema name (MkJSONSchema defs constraints) = do
        writeDefs defs
        writeSchemaConstraints name constraints

    writeSchemaConstraints : QTypeName -> JSONSchemaConstraints QTypeName -> Writer IdrisModule ()
    writeSchemaConstraints name (JSObject props) = do
        propNames <- namespaceBlock (shortName name) $ for props $ \(MkJSONPropertySchema propName propSchema) => do
            ref <- refSchema (name <.> asIdrisTypeName propName) propSchema
            pure (propName, ref)
        addLines [<
            "public export",
            "record \{show $ shortName name} where" ,
            "    constructor \{show $ constructorName $ shortName name}"
          ]
        for_ propNames $ \(propName, ref) => do
            addLines [<"    \{show $ asIdrisPropName propName} : \{ref}"]
    writeSchemaConstraints name (JSEnum options) = do
        addLines [<
            "public export",
            "data \{show $ shortName name} = " ++ (concat $ intersperse " | " $ map show $ constructorNames (shortName name) $ map jsonAsName options)
          ]
    writeSchemaConstraints name (JSAnyOf schemas) = do
        variants <- namespaceBlock (shortName name) $ for (genNames (shortName name) schemas) $ \(conName, typeName, schema) => do
            ref <- refSchema (name <.> typeName) schema {asSubexpression = True}
            pure $ show conName ++ " " ++ ref
        addLines [<
            "public export",
            "data \{show $ shortName name} = " ++ (concat $ intersperse " | " variants)
          ]
    writeSchemaConstraints name schema = do
        ref <- refSchemaConstraints name schema
        addLines [<
            "public export",
            "\{show $ shortName name} : Type" ,
            "\{show $ shortName name} = \{ref}"
          ]

    ||| Get a potentially-anonymous reference to the type described by a schema
    ||| The type can use the given name to construct itself, if necessary
    refSchema : QTypeName
             -> JSONSchema QTypeName
             -> {default False asSubexpression : Bool}
             -> Writer IdrisModule String
    refSchema name (MkJSONSchema defs constraints) = do
        writeDefs defs
        refSchemaConstraints name constraints {asSubexpression}

    refSchemaConstraints : QTypeName
                        -> JSONSchemaConstraints QTypeName
                        -> {default False asSubexpression : Bool}
                        -> Writer IdrisModule String
    refSchemaConstraints _ (JSAtom atomSchema) = pure $ asIdrisType atomSchema
    refSchemaConstraints name (JSArray itemSchema) = do
        let itemName = subName (shortName name) "Item"
        ref <- namespaceBlock (shortName name) $ refSchema (name <.> itemName) itemSchema {asSubexpression = True}
        if asSubexpression
            then pure $ "(List \{ref})"
            else pure $ "List \{ref}"
    refSchemaConstraints name (JSRef ref) = pure $ show ref
    refSchemaConstraints _ JSAny = do
        addImport "Language.JSON"
        pure "JSON"
    refSchemaConstraints name schema = do
        writeSchemaConstraints name schema
        addLines [<""]
        pure $ show name

    writeDefs : SortedMap QTypeName (JSONSchema QTypeName) -> Writer IdrisModule ()
    writeDefs defs = do
        let deps = map support defs
        for_ (topoSort deps) $ \case
            name ::: [] =>
                let isCircularDef = maybe False (contains name) $ lookup name deps in
                (ifThenElse isCircularDef mutualBlock id) $ writeDef name
            names => mutualBlock $ traverse_ writeDef names
      where
        writeDef : QTypeName -> Writer IdrisModule ()
        writeDef name = case lookup name defs of
            Nothing => pure ()
            Just schema => do
                writeSchema name schema
                addLines [<""]

export
compileSchema : JSONSchema QTypeName -> List String
compileSchema schema = cast {from = SnocList String} $ execWriter $ do
    let idrisModule = execWriter $ writeSchema (global "Main") schema

    case SortedSet.toList $ imports idrisModule of
        [] => pure ()
        xs => tell $ cast (map ("import " ++) xs) :< ""

    tell $ lines idrisModule
