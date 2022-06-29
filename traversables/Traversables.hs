module Traversables where 



class (Functor t , Foldable t ) => Traversables t where 
    traverses :: Applicative f => (a -> f b) -> t a -> f (t b)  
    

sequenceA ::(Traversables t,  Applicative f) => t (f a) -> f (t a)
sequenceA = traverses  id 


-- traverses :: Applicative f => (a -> f b) -> t a -> f (t b) 
-- traverses = sequenceA . fmap g 