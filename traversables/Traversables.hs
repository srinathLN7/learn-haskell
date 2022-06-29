module Traversables where 
class (Functor t , Foldable t ) => Traversables t where 
    traverses :: Applicative f => (a -> f b) -> t a -> f (t b)  
    

sequenceA ::(Traversables t,  Applicative f) => t (f a) -> f (t a)
sequenceA = traverses  id 


instance Traversables [] where 
    -- traverses :: Applicative f => (a -> f b) -> [a]-> f ([b]) 

    traverses _ [] = pure []
    traverses g (x:xs) = pure (:) <*> g x <*> traverses g xs  

instance Traversables Maybe where
    -- traverses :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
    traverses _ Nothing = pure (Nothing)
    traverses g (Just x) = pure (Just) <*> g x 


data Tree a = Leaf a | Node (Tree a) (Tree a)  deriving Show 

instance Functor Tree where
    -- fmaps :: (a -> b) -> Tree a -> Tree b
    fmap g (Leaf x) = Leaf (g x)
    fmap g (Node l r) = Node (fmap g l) (fmap g r)
instance Foldable Tree where 
     -- foldMap :: Monoids b => (a -> b) -> Tree a -> b 
    foldMap f (Leaf x) = f x 
    foldMap f (Node l r) = foldMap f l `mappend` foldMap f r

instance Traversables Tree   where
    -- traverses :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
    traverses g (Leaf x) = pure Leaf <*> g x
    traverses g (Node l r) = pure Node <*> traverses g l <*> traverses g r  


dec :: Int -> Maybe Int
dec n = if n >0 then Just (n-1) else Nothing


mapM :: (Traversables t ,Monad m) => (a -> m b) -> t a -> m (t b)
mapM = traverses

sequenceM :: (Traversables t ,Monad m) => t (m a) -> m (t a)
sequenceM = Traversables.sequenceA 