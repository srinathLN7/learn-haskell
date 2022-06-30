module Monoids.Foldables where
import Prelude hiding (Maybe, Nothing, Just, foldMap, foldr, foldl)
import Monoids 

-- To avoid ambiguous error from the Haskell prelude module 
-- Foldables === Foldable; 

class Foldables t where 
    fold :: Monoids a => t a -> a 
    foldMap :: Monoids b => (a -> b) -> t a -> b 
    foldr :: (a -> b -> b) -> b -> t a -> b 
    foldl :: (a -> b -> a) -> a -> t b -> a
    

-- List as foldables    
instance Foldables [] where
    -- fold :: Monoids a => [a] -> a 
    fold [] = memptys
    fold (x:xs) = x `mappends` fold xs -- foldr mappends memptys

    -- foldMap :: Monoids a => (a -> b) -> [a] -> b 
    foldMap _ [] = memptys
    foldMap f (x:xs) = f x `mappends` foldMap f xs 

    -- foldr :: (a -> b -> b) -> b -> [a] -> b 
    -- Head is processed at the last
    foldr _ v [] = v 
    foldr f v (x:xs) = f x (foldr f v xs) 

    -- foldl :: (a- > b -> a) -> a -> [b] -> a
    -- Head is processed first
    foldl _ v [] = v
    foldl f v (x:xs) = foldl f (f v x) xs


-- Maybe as Foldables

data Maybe a = Nothing | Just a 
instance Foldables Maybe where 
    -- fold :: Monoid a => Maybe a -> a 
        fold Nothing = memptys  
        fold (Just x) = x 

    -- foldMap :: Monoid b => (a -> b) -> Maybe a -> b
        foldMap _ Nothing = memptys  
        foldMap g (Just x) = g x   

    -- foldr :: (a -> b -> b) -> b -> Maybe a -> b
        foldr _ v Nothing =  v 
        foldr g v (Just x) = g x (foldr g v (Just x))

    -- foldl :: (a -> b -> a) -> a -> Maybe b -> a
        foldl _ v Nothing = v 
        foldl g v (Just x) = foldl g (g v x) (Just x)

-- Defining a foldable binary tree
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

instance Foldables Tree where 
    -- fold :: Monoids a => Tree a -> a 
    fold (Leaf x) = x 
    fold (Node l r) = fold l `mappends` fold r

    -- foldMap :: Monoids b => (a -> b) -> Tree a -> b 
    foldMap f (Leaf x) = f x 
    foldMap f (Node l r) = foldMap f l `mappends` foldMap f r

    -- foldr :: (a -> b -> b) -> b -> Tree a -> b
    -- process the right tree first 
    foldr f v (Leaf x) = f x v
    foldr f v (Node l r) = foldr f (foldr f v r) l 

    -- foldl :: (a -> b -> a) -> a -> Tree b -> a
    -- process the left tree first
    foldl f v (Leaf x) = f v x
    foldl f v (Node l r) = foldl f (foldl f v l) r  

    -- Example to test this binary tree
tree :: Tree Int 
tree = Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)    

-- Define map in terms of foldr 
map' :: (a -> b) -> [a] -> [b]
map' f  = foldr ((:).f) [] 

-- Generic function of the filter function working with any foldable type
-- filter :: (a -> Bool) -> [a] -> [a] 
-- foldMap :: Monoid b => (a -> b) -> t a -> b

filterF :: Foldables t => (a -> Bool) -> t a -> [a]
filterF p = foldMap (\x -> if p x then [x] else [])

-- toList :: t a -> [a]
-- filterF p ta = filter p (toList ta)


average :: Foldable t => t Int -> Int  
average xs = sum xs `div` length xs


-- define `and` operator for a generic foldable data structure  in terms of the `All` monoid
-- newtype All = All {c ::Bool}
and :: Foldables t => t Bool -> Bool 
and = c. foldMap All

-- define `or` operator for a generic foldable data structure  in terms of the `Any` monoid
-- newtype Any  = Any {d :: Bool} 
or :: Foldables t => t Bool -> Bool 
or = d. foldMap Any 


-- check if all elements in a generic foldable data structure satisifies a predicate
-- newtype All = All {c ::Bool}
all :: Foldables t => (a -> Bool) -> t a -> Bool 
all p = c. foldMap (All. p)


-- check if all elements in a generic foldable data structure satisifies a predicate
-- newtype Any = Any {d ::Bool}
any :: Foldables t => (a -> Bool) -> t a -> Bool 
any p = d. foldMap (Any. p)


-- define fmap in terms of foldr
-- foldMap :: Monoids b => (a -> b) -> t a -> b 
-- foldMap f = foldr (mappends. f) memptys
