data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show)
--data [a] = [] | (a:[a])


map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map f xs

treeMap':: (a -> b) -> Tree a -> Tree b
treeMap' f (Leaf x) = Leaf (f x)
treeMap' f (Branch lt rt) = Branch (treeMap' f lt) (treeMap' f rt)

tree1 :: Tree Integer
tree1 = 
    Branch
       (Branch 
           (Branch 
               (Leaf 1) 
               (Branch (Leaf 2) (Leaf 3))) 
           (Branch 
               (Leaf 4) 
               (Branch (Leaf 5) (Leaf 6)))) 
       (Branch
           (Branch (Leaf 7) (Leaf 8)) 
           (Leaf 9))