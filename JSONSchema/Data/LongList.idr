module Data.LongList

import Language.JSON

%default total

public export
data LongList : Nat -> Type -> Type where
    Nil : LongList 0 a
    (::) : a -> LongList (minus n 1) a -> LongList n a

%name LongList xs, ys

public export
Functor (LongList n) where
    map f [] = []
    map f (x :: xs) = f x :: map f xs

public export
Foldable (LongList n) where
    foldr f x [] = x
    foldr f x (y :: xs) = f y (foldr f x xs)

    foldl f x [] = x
    foldl f x (y :: xs) = foldl f (f x y) xs

public export
Cast a JSON => Cast (LongList n a) JSON where
    cast x = cast (toList x)

public export
checkLength : {n : Nat} -> List a -> Maybe (LongList n a)
checkLength {n = 0} [] = Just []
checkLength {n = S n} [] = Nothing
checkLength (x :: xs) = map (x ::) (checkLength xs)
