module JSONSchema.Compiler.Data

import public Control.Monad.Identity
import public Control.Monad.Writer
import public Data.SnocList

import public Libraries.Data.SortedSet

import JSONSchema.Names

%default total

public export
record IdrisModule where
    constructor MkIdrisModule
    imports : SortedSet String
    dataLines : SnocList String
    castLines : SnocList String

export
Semigroup IdrisModule where
    x <+> y = MkIdrisModule
        (union y.imports x.imports)
        (x.dataLines <+> y.dataLines)
        (x.castLines <+> y.castLines)

export
Monoid IdrisModule where
    neutral = MkIdrisModule empty [<] [<]

export
addImport : String -> Writer IdrisModule ()
addImport imp = tell $ MkIdrisModule (singleton imp) [<] [<]

export
addDataLines : SnocList String -> Writer IdrisModule ()
addDataLines lines = tell $ MkIdrisModule empty lines [<]

export
addCastLines : SnocList String -> Writer IdrisModule ()
addCastLines lines = tell $ MkIdrisModule empty [<] lines

indentLine : String -> String
indentLine "" = ""
indentLine line = "    " ++ line

block : String -> Writer IdrisModule a -> Writer IdrisModule a
block name = mapWriter $ \(x, m) => (x, MkIdrisModule m.imports (block' name m.dataLines) (block' name m.castLines))
  where
    block' : String -> SnocList String -> SnocList String
    block' name [<] = [<]
    block' name xs = [<name] ++ map indentLine xs

export
mutualBlock : Writer IdrisModule a -> Writer IdrisModule a
mutualBlock = block "mutual"

export
namespaceBlock : TypeName -> Writer IdrisModule a -> Writer IdrisModule a
namespaceBlock name = block "namespace \{show name}"
