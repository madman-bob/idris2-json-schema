module JSONSchema.Compiler.Data

import public Control.Monad.Identity
import public Control.Monad.Writer
import public Data.SnocList

import public Libraries.Data.SortedSet

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

export
indent : Writer IdrisModule a -> Writer IdrisModule a
indent = mapLines $ \case
    "" => ""
    line => "    " ++ line

export
mutualBlock : Writer IdrisModule a -> Writer IdrisModule a
mutualBlock writer = do
    addLines [<"mutual"]
    indent writer
