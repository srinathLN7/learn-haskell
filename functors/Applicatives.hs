{-# LANGUAGE DeriveFunctor #-}
module Applicatives where
import Functors 

--- Since class Applicative is already defined in the prelude module, we rewrite the same definition using class Applicative to avoid ambiguous errors
--- Hence in this module Applicatives === Applicative ; 
-- fmaps === fmap;
-- fmaps :: (a -> b) -> f a -> f b 


-- Applicative definition

class Functors f => Applicatives f where 
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b 

{-  
Common instances of the Functors classes 
1. Maybe
2. List
3. IO
-}


-- Applicatives declarations

-- Maybe as Applicatives
instance Applicatives Maybe where 
    
    -- pure :: a -> Maybe a
    pure  = Just

    -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
    Nothing <*> _ = Nothing
    Just g <*> mx = fmap g mx  



-- List as Applicatives
instance Applicatives [] where
    -- pure :: a -> [a]
    pure x = [x]

    -- (<*>) :: [a -> b] -> [a] -> [b]
    gs <*> xs = [g x | g <- gs, x <- xs]


 -- Example of product of lists in applicative style without having to calculate intermediate result

prods :: Num a => [a] -> [a] -> [a]
prods xs ys = Applicatives.pure (*)  Applicatives.<*> xs Applicatives.<*> ys
-- prods xs ys = [x * y | x <- xs , y <- ys]


-- IO as Applicatives

instance Applicatives IO where
    -- pure :: a -> IO a
    pure = return

    -- (<*>) :: f (a -> b) -> IO a -> IO b
    mg <*> mx = do {g <- mg ; x <-  mx ; return (g x)}


 -- Examples of IO Applicative

sequenceA :: Applicatives f => [f a] -> f [a]
sequenceA [] = Applicatives.pure []
sequenceA (x:xs) = Applicatives.pure (:) Applicatives.<*> x Applicatives.<*> Applicatives.sequenceA xs 

getChars :: Int -> IO String
getChars n = Applicatives.sequenceA (replicate n getChar)

-- Applicatvies can be used to extend of fmap to any number of arguments 

-- fmap0 :: a -> f a 
-- fmap0 = pure
-- fmap1 :: (a -> b) -> f a -> f b 
-- fmap1 g mx = pure g <*> mx === g <$> mx = fmap g x 
-- fmap2 :: (a -> b -> c) -> f a -> f b -> f c
-- fmap2 g mx my = pure g <*> mx <*> my === g <$> mx <*> my
-- fmap3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d
-- fmap 3 g mx my mz = pure g <*> mx <*> my <*> mz === g <$> mx <*> my <*> mz


data Employee = Employee {name :: String, phone :: String} deriving Show 

m_name1 , m_name2 :: Maybe String
m_name1 = Nothing 
m_name2 = Just "Srinath"

m_phone1 , m_phone2 :: Maybe String
m_phone1 = Nothing 
m_phone2 = Just "0021"


exA = Applicatives.pure Employee Applicatives.<*> m_name1 Applicatives.<*> m_phone1
exB = Applicatives.pure Employee Applicatives.<*> m_name1 Applicatives.<*> m_phone2
exC = Applicatives.pure Employee Applicatives.<*> m_name2 Applicatives.<*> m_phone1
exD = Applicatives.pure Employee Applicatives.<*> m_name2 Applicatives.<*> m_phone2

names = ["Jones", "Sara", "Ali"]
phones = ["001", "002", "003"]

employeesAll = Applicatives.pure Employee Applicatives.<*> names Applicatives.<*> phones 


-- Let us zip the names and phones element-wise

newtype ZipList a = ZipList {getZipList :: [a]} deriving (Eq, Show, Functor)

instance Applicative ZipList where 
   -- pure :: a -> ZipList a 
   pure  = ZipList . repeat 

   -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
   ZipList fs <*> ZipList xs = ZipList [ f x | (f, x) <- zip fs xs]

employeesSpecific = getZipList $ Employee Prelude.<$> ZipList names Prelude.<*> ZipList phones  