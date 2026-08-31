module Data.ShortList

import Language.JSON

%default total

public export
data ShortList : Nat -> Type -> Type where
    Nil : ShortList n a
    (::) : a -> ShortList n a -> ShortList (S n) a

%name ShortList xs, ys

public export
Functor (ShortList n) where
    map f [] = []
    map f (x :: xs) = f x :: map f xs

public export
Foldable (ShortList n) where
    foldr f x [] = x
    foldr f x (y :: xs) = f y (foldr f x xs)

    foldl f x [] = x
    foldl f x (y :: xs) = foldl f (f x y) xs

public export
Cast a JSON => Cast (ShortList n a) JSON where
    cast x = cast (toList x)

public export
checkLength : {n : Nat} -> List a -> Maybe (ShortList n a)
checkLength [] = Just []
checkLength {n = 0} (x :: xs) = Nothing
checkLength {n = S n} (x :: xs) = map (x ::) (checkLength xs)
