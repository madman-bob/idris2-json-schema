module JSONSchema.JSONUtils

import Data.List

import Language.JSON

%default total

export
lookup : (key : String) -> JSON -> Maybe JSON
lookup key (JObject xs) = lookup key xs
lookup key _ = Nothing
