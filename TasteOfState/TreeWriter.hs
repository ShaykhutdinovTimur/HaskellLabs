module TreeWriter where
import Control.Monad.Writer

data Tree a = Leaf | Node a (Tree a) (Tree a)

find :: (Ord a, Show a) => a -> Tree a -> Writer String (Tree a)
find k Leaf = writer (Leaf, "not found " ++ show k ++ "\n")
find k t@(Node x l r)
    | k == x = writer (t, "found " ++ show k ++ "\n")
    | k < x = find k l
    | k > x = find k r

insert :: (Ord a) => a -> Tree a -> Writer String (Tree a)
insert k Leaf = writer (Node k Leaf Leaf, "inserted\n")
insert k (Node x l r)
    | k == x = writer (Node x l r, "not inserted\n")
    | k < x = (insert k l) >>= \g -> writer (Node x g r, "")
    | k > x = (insert k l) >>= \g -> writer (Node x l g, "")

delete :: (Ord a, Show a) => a -> Tree a -> Writer String (Tree a)
delete k Leaf = writer (Leaf, show k ++ " not deleted")
delete k t@(Node x l r)
    | k > x = (delete k r) >>= \g -> writer (Node x l g, "")
    | k < x = (delete k r) >>= \g -> writer (Node x g r, "")
    | k == x = writer (deleteRoot t, "deleted " ++ show k)

deleteRoot :: (Ord a, Show a) => Tree a -> Tree a
deleteRoot (Node _ l Leaf) = l
deleteRoot (Node _ Leaf r) = r
deleteRoot (Node _ l r) = Node v l u where
    v = lowest r
    u = fst $ runWriter (delete v r)

lowest :: (Ord a) => Tree a -> a
lowest (Node x Leaf _) = x
lowest (Node _ l _) = lowest l

toList :: Tree a -> [a]
toList Leaf = []
toList (Node x l r) = x : ((toList l) ++ (toList r))

fromList :: (Ord a) => [a] -> Writer String (Tree a)
fromList [] = writer (Leaf, "created tree\n")
fromList (x:xs) = (fromList xs) >>= \t -> insert x t >>= \l -> writer (l, "")