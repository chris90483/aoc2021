module Infinitable where
    data Infinitable a = Infinity | Finite a deriving Show

    instance Eq a => Eq (Infinitable a) where
        (==) Infinity Infinity     = True
        (==) Infinity (Finite x)   = False
        (==) (Finite x) Infinity   = False
        (==) (Finite x) (Finite y) = x == y
        (/=) Infinity Infinity     = False
        (/=) Infinity (Finite x)   = True
        (/=) (Finite x) Infinity   = True
        (/=) (Finite x) (Finite y) = x /= y

    instance Ord a => Ord (Infinitable a) where
        compare Infinity Infinity     = EQ
        compare Infinity (Finite x)   = GT
        compare (Finite x) Infinity   = LT
        compare (Finite x) (Finite y) | x > y  = GT
                                      | x == y = EQ
                                      | x < y  = LT