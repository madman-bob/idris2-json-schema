module Data.BoundedList

import Language.JSON

%default total

public export
data BoundedList : Nat -> Nat -> Type -> Type where
    Nil : BoundedList 0 m a
    (::) : a -> BoundedList (minus n 1) m a -> BoundedList n (S m) a

%name BoundedList xs, ys

public export
Functor (BoundedList n m) where
    map f [] = []
    map f (x :: xs) = f x :: map f xs

public export
Foldable (BoundedList n m) where
    foldr f x [] = x
    foldr f x (y :: xs) = f y (foldr f x xs)

    foldl f x [] = x
    foldl f x (y :: xs) = foldl f (f x y) xs

public export
Cast a JSON => Cast (BoundedList n m a) JSON where
    cast x = cast (toList x)

public export
checkLength : {n : Nat} -> {m : Nat} -> List a -> Maybe (BoundedList n m a)
checkLength {n = 0} [] = Just []
checkLength {n = S n} [] = Nothing
checkLength {m = 0} (x :: xs) = Nothing
checkLength {m = S m} (x :: xs) = map (x ::) (checkLength xs)
