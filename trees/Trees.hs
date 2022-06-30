module Trees where
import Data.Foldable

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show 

-- make this Tree as instance of Foldable class 
instance Foldable Tree where
    -- fold :: Monoid a => Tree a -> a 
    fold Leaf = mempty
    fold (Node l x r) = fold l <> x <> fold r

    -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
    foldMap _ Leaf = mempty
    foldMap g (Node l x r) = foldMap g l <> g x <> foldMap g r

    -- foldr :: (a -> b -> b) -> b -> Tree a -> b 
    -- fold the right tree first
    foldr _ v Leaf = v
    foldr g v (Node l x r) = foldr g (foldr g (g x v) r) l


    -- foldl :: (a -> b -> a) -> a -> Tree b -> a 
    -- fold the left tree first
    foldl _ v Leaf = v 
    foldl g v (Node l x r) = foldl g (foldl g (g v x) l) r


-- To make this tree an instance of the traversable class

-- For this, first make the tree an instance of the Functor class
instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap _ Leaf = Leaf
    fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

-- make this tree an instance of the traversable class
instance Traversable Tree where
    -- traverse :: Appplicative f => (a -> f b) -> Tree a -> f (Tree b)  
    traverse _ Leaf = pure Leaf
    traverse g (Node l x r) = pure Node <*> traverse g l <*> g x <*> traverse g r 

-- Example to test this binary tree

tree1 :: Tree Int 
tree1 = Node (Node Leaf 2 Leaf) 1 (Node Leaf 3 Leaf)

tree2 :: Tree Int 
tree2 = Node (Node Leaf 1 Leaf) 0 (Node Leaf 2 Leaf)

dec :: Int -> Maybe Int
dec n = if n >0 then Just (n-1) else Nothing