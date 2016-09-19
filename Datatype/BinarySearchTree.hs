module BinarySearchTree where
    import TreePrinters

  --data Tree a = Leaf | Node a (Tree a) (Tree a)
  --find
  --insert
  --delete
  --toList
  --fromList


    find :: (Ord a) => a -> Tree a -> Tree a
    find k Leaf = Leaf
    find k t@(Node x l r)
        | k == x = t
        | k < x = find k l
        | k > x = find k r

    insert :: (Ord a) => a -> Tree a -> Tree a
    insert k Leaf = Node k Leaf Leaf
    insert k t@(Node x l r)
        | k == x = Node x l r
        | k < x = Node x (insert k l) r
        | k > x = Node x l (insert k r)

    delete :: (Ord a) => a -> Tree a -> Tree a
    delete k Leaf = Leaf
    delete k t@(Node x l r)
        | k > x = Node x l (delete k r)
        | k < x = Node x (delete k l) r
        | k == x = deleteRoot t

    deleteRoot :: (Ord a) => Tree a -> Tree a
    deleteRoot (Node x l Leaf) = l
    deleteRoot (Node x Leaf r) = r
    deleteRoot (Node x l r) = Node v l u where
        v = lowest r
        u = delete v r

    lowest :: (Ord a) => Tree a -> a
    lowest (Node x Leaf _) = x
    lowest (Node _ l _) = lowest l

    toList :: Tree a -> [a]
    toList Leaf = []
    toList (Node x l r) = x : ((toList l) ++ (toList r))

    fromList :: (Ord a) => [a] -> Tree a
    fromList [] = Leaf
    fromList (x:xs) = insert x (fromList xs)
