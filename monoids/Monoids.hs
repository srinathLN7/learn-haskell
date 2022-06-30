module Monoids where 

-- To avoid ambiguous error from the Prelude module
-- Monoids === Monoid ; -- memptys === mempty;
-- mappends === mappend; -- mconcats === mconcat

class Monoids a where 
     memptys :: a
     mappends :: a -> a -> a 

     mconcats :: [a] -> a 
     mconcats = foldr mappends memptys 



-- Monoid Laws 
-- mempty `mappend` x = x 
-- x `mappend` mempty = x 
-- x `mappend` (y `mappend` z) = (x `mappend` y) `mappend` z   


-- List as Monoid 

instance Monoids [a] where 
    -- memptys :: [a]
    memptys = []

    mappends = (++)


-- Maybe as Monoid 
-- For Maybe to be a monoid the parameter a should also be a monoid

instance Monoids a => Monoids (Maybe a) where
 -- memptys :: Maybe 
 memptys = Nothing

 -- mappends :: Maybe a -> Maybe a -> Maybe a 
 Nothing `mappends` my = my
 mx `mappends` Nothing = mx
 Just x `mappends` Just y = Just (x `mappends` y) 


 -- Since `Int` forms a Monoid both under addition and multiplication and Haskell allows only declaration of the same type
 -- we can define a wrapper function 

-- Addition operator as Monoid class

-- a :: Add a -> a
newtype Add a = Add {a :: a} deriving (Eq, Ord, Show, Read)
 
instance Num a => Monoids (Add a) where 
  -- memptys :: Add a 
  memptys = Add 0   

  -- mappends :: Add a -> Add a -> Add a
  Add a `mappends` Add b = Add (a + b)  


-- Multiplication operator as Monoid class

-- b :: Mult b -> b
newtype Mult b = Mult {b :: b} deriving (Eq, Ord, Show, Read)

instance Num b => Monoids (Mult b) where
    -- meemptys :: Mult b 
    memptys = Mult 1 

    -- mappends :: Mult b -> Mult b -> Mult b
    Mult a `mappends` Mult b = Mult (a *b)


-- And operator as Monoid class
-- c :: All c -> Bool 

newtype All = All {c ::Bool} deriving (Show, Read)  

instance Monoids All where
    -- memptys :: All c 
    memptys = All True  

    -- mappends :: All c -> All c -> All c 
    All x `mappends` All y = All (x && y) 


-- Or operator as Monoid class
-- d :: Any d -> Bool 

newtype Any  = Any {d :: Bool} deriving (Show, Read)

instance Monoids Any  where 
    -- memptys :: Any d 
    memptys = Any False

    -- mappends :: Any d -> Any d -> Any d 
    Any x `mappends` Any y =  Any (x || y) 


-- Make a pair (a,b) in to Monoids
-- a and b are Monoids
instance (Monoids e, Monoids f) => Monoids (e,f) where
    -- memptys :: (e,f)
    memptys = (memptys,memptys)

    -- mappends :: (e, f) -> (e, f) -> (e, f)
    (x1, y1) `mappends` (x2, y2) = (x1 `mappends` x2 , y1 `mappends` y2)


-- Make (a -> b) into a Monoid
-- b is a monoid 
instance Monoids b => Monoids (a -> b) where
    -- memptys :: (a -> b)
       memptys = \ _ -> memptys    

    -- mappends :: (a -> b) -> (a -> b) -> (a -> b)
       g1 `mappends` g2 = \ x -> g1 x `mappends` g2 x 







--  mappends -> infix notation <>
-- x <> y = x `mappends` y