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
    lines : SnocList String

export
Semigroup IdrisModule where
    (MkIdrisModule imports lines) <+> (MkIdrisModule imports' lines') = MkIdrisModule (union imports' imports) (lines <+> lines')

export
Monoid IdrisModule where
    neutral = MkIdrisModule empty [<]

export
addImport : String -> Writer IdrisModule ()
addImport imp = tell $ MkIdrisModule (singleton imp) [<]

export
addLines : SnocList String -> Writer IdrisModule ()
addLines lines = tell $ MkIdrisModule empty lines

export
mapLines : (String -> String) -> Writer IdrisModule a -> Writer IdrisModule a
mapLines f = mapWriter $ \(x, MkIdrisModule imports lines) => (x, MkIdrisModule imports (map f lines))

indentLine : String -> String
indentLine "" = ""
indentLine line = "    " ++ line

export
indent : Writer IdrisModule a -> Writer IdrisModule a
indent = mapLines indentLine

export
mutualBlock : Writer IdrisModule a -> Writer IdrisModule a
mutualBlock writer = do
    addLines [<"mutual"]
    indent writer

export
namespaceBlock : TypeName -> Writer IdrisModule a -> Writer IdrisModule a
namespaceBlock name =
    mapWriter $ \case
        (x, MkIdrisModule imports [<]) => (x, MkIdrisModule imports [<])
        (x, MkIdrisModule imports lines) => (x, MkIdrisModule imports ([<"namespace \{show name}"] ++ map indentLine lines))
