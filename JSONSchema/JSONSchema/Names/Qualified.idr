module JSONSchema.Names.Qualified

import Data.List
import Data.SnocList

import JSONSchema.Names.PropName
import JSONSchema.Names.TypeName

||| Qualified type name
export
record QTypeName where
    constructor MkQTypeName
    parents : SnocList TypeName
    name : TypeName

asSList : QTypeName -> SnocList TypeName
asSList (MkQTypeName parents name) = parents :< name

export
Eq QTypeName where
    (==) = (==) `on` asSList

export
Ord QTypeName where
    compare = compare `on` asSList

export
Show QTypeName where
    show (MkQTypeName parents name) = concat $ intersperse "." $ cast $ map show parents :< show name

export
global : TypeName -> QTypeName
global name = MkQTypeName [<"Main"] name

export
shortName : QTypeName -> TypeName
shortName (MkQTypeName parents name) = name

infixl 5 <.>

export
(<.>) : QTypeName -> TypeName -> QTypeName
(MkQTypeName parents name) <.> child = MkQTypeName (parents :< name) child
