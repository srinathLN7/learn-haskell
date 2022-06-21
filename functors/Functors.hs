module Functors where

--- Since class Functor is already defined in the prelude module, we rewrite the same definition using class Functors to avoid ambiguous errors
--- Hence in this module Functors === Functor ; fmaps === fmap

-- For a parameterized type f to be an instance of the Functors class 
-- it must implement the function fmaps

class Functors f where
    fmaps :: (a -> b) -> f a -> f b

{-  
Common instances of the Functors classes 
1. List
2. Maybe
3. Tree
4. IO
-}

-- Functors declarations


-- List Functors
instance Functors [] where
    -- fmaps :: (a -> b) -> [a] -> [b]
    fmaps = map 


-- Maybe Functors
instance Functors Maybe where
    -- fmaps :: (a -> b) -> Maybe a -> Maybe b
    fmaps g Nothing  = Nothing
    fmaps g (Just x) = Just (g x)


-- Tree as Functors

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)


instance Functors Tree where
    -- fmaps :: (a -> b) -> Tree a -> Tree b
    fmaps g (Leaf x) = Leaf (g x)
    fmaps g (Node l r) = Node (fmaps g l) (fmaps g r)


-- IO as Functors

instance Functors IO where
    -- fmaps :: (a -> b) -> IO a -> I) b
    fmaps g mx = do {x <- mx; return (g x)} 
                    -- g <$> mx 


-- A generic increment functor which would work for all parametric types
inc :: (Functors f, Num a) => f a -> f a 
inc = fmaps (+1) 


-- FUNCTOR LAWS

-- fmaps id = id
-- fmaps (g. h) = fmap g . fmap h

--- SOME EXAMPLES
 
       