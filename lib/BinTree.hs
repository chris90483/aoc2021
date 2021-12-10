module BinTree where
    data BinTree a = Leaf | Branch a (BinTree a) (BinTree a) deriving Show

    -- map stuff
    binSearch :: (Eq k, Ord k) => k -> BinTree (k, v) -> Maybe v
    binSearch x Leaf                               = Nothing
    binSearch x (Branch (k, v) t1   t2  ) | x == k = Just v
    binSearch x (Branch (k, v) Leaf t2  )          = binSearch x t2
    binSearch x (Branch (k, v) t1   Leaf)          = binSearch x t1
    binSearch x (Branch (k, v) (Branch k1 t11 t12) (Branch k2 t21 t22)) | x > k     = binSearch x (Branch k2 t21 t22)
    binSearch x (Branch (k, v) (Branch k1 t11 t12) (Branch k2 t21 t22)) | otherwise = binSearch x (Branch k1 t11 t12)

    insertTMap :: (Eq k, Ord k) => (k, v) -> BinTree (k, v) -> BinTree (k, v)
    insertTMap (a, b) Leaf                                  = Branch (a, b) Leaf                      Leaf
    insertTMap (a, b) (Branch (k, v) Leaf Leaf) | a > k     = Branch (k, v) Leaf                      (Branch (a, b) Leaf Leaf)
    insertTMap (a, b) (Branch (k, v) Leaf Leaf) | otherwise = Branch (k, v) (Branch (a, b) Leaf Leaf) Leaf
    insertTMap (a, b) (Branch (k, v) Leaf t2)   | a > k     = Branch (k, v) Leaf                      (insertTMap (a, b) t2)
    insertTMap (a, b) (Branch (k, v) Leaf t2)   | otherwise = Branch (k, v) (Branch (a, b) Leaf Leaf) t2
    insertTMap (a, b) (Branch (k, v) t1   Leaf) | a > k     = Branch (k, v) t1                        (Branch (a, b) Leaf Leaf)
    insertTMap (a, b) (Branch (k, v) t1   Leaf) | otherwise = Branch (k, v) (insertTMap (a, b) t1)       Leaf
    insertTMap (a, b) (Branch (k, v) t1 t2)     | a > k     = Branch (k, v) t1                        (insertTMap (a, b) t2)
    insertTMap (a, b) (Branch (k, v) t1 t2)     | otherwise = Branch (k, v) (insertTMap (a, b) t1)       t2

    -- sort stuff
    insertT :: (Ord a) => a -> BinTree a -> BinTree a
    insertT a Leaf                             = Branch a Leaf                      Leaf
    insertT a (Branch b Leaf Leaf) | a > b     = Branch b Leaf                      (Branch a Leaf Leaf)
    insertT a (Branch b Leaf Leaf) | otherwise = Branch b (Branch a Leaf Leaf) Leaf
    insertT a (Branch b Leaf t2)   | a > b     = Branch b Leaf                      (insertT a t2)
    insertT a (Branch b Leaf t2)   | otherwise = Branch b (Branch a Leaf Leaf) t2
    insertT a (Branch b t1   Leaf) | a > b     = Branch b t1                        (Branch a Leaf Leaf)
    insertT a (Branch b t1   Leaf) | otherwise = Branch b (insertT a t1)       Leaf
    insertT a (Branch b t1 t2)     | a > b     = Branch b t1                        (insertT a t2)
    insertT a (Branch b t1 t2)     | otherwise = Branch b (insertT a t1)       t2

    toBinTree :: (Ord a) => [a] -> BinTree a
    toBinTree xs = tbt xs Leaf
        where
            tbt []     t = t
            tbt (x:xs) t = tbt xs (insertT x t)

    toList :: (Ord a) => BinTree a -> [a]
    toList Leaf             = []
    toList (Branch v t1 t2) = (toList t1) ++ [v] ++ (toList t2)

    binTreeSort :: (Ord a) => [a] -> [a]
    binTreeSort xs = toList $ toBinTree xs

