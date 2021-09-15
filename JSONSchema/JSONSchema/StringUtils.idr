module JSONSchema.StringUtils

import Data.List

%default total

export
filter : (Char -> Bool) -> String -> String
filter f str = pack $ filter f $ unpack str

export
title : String -> String
title str = pack $ filter (/= '_') $ title' True $ unpack str
  where
    title' : Bool -> List Char -> List Char
    title' _ [] = []
    title' True (x :: xs) = toUpper x :: title' (not $ isAlpha x) xs
    title' False (x :: xs) = x :: title' (not $ isAlpha x) xs

export
camelCase : String -> String
camelCase str = case unpack $ title str of
    [] => ""
    x :: xs => pack $ toLower x :: xs
