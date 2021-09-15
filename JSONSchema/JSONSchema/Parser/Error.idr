module JSONSchema.Parser.Error

%default total

export
allOk : List (Maybe a) -> Maybe (List a)
allOk [] = Just []
allOk (x :: xs) = pure (!x :: !(allOk xs))
