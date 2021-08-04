powersOf2' n = [2^i | i <- [0..n]]  -- enumFromTo 0 n = [0..n]

-- Rewrite some of the inbuilt functions in Haskell
head' :: [a] -> a
head' [] = error "Empty list"
head' (x:_)=x

tail' :: [a] -> [a]
tail' [] = []
tail' (_:xs)=xs

fst' :: (a,b) -> a
fst' (a,_)= a 

snd' :: (a,b) -> b
snd' (_, b)= b 

zip' :: [a] -> [b] ->[(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys


-- sum function
sum' :: Num  a => [a] ->a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- sort Function 

sort' :: Ord a => [a] -> [a]
sort' [] = []
sort' (x:xs) = sort' left ++ [x] ++ sort' right
      where left= [z | z <-xs, z<=x]
            right=[z | z <-xs, z>x]


-- watched make valley problem
-- makeValley' (x:y:xs) = x : makeValley' xs ++ y
 
-- sum using accumulator function
sum'' :: Num a => (a->a->a) -> a -> [a] ->a 
sum'' _ total [] = total
sum'' fn total (x:xs) = sum'' fn (fn total  x) xs -- sum'' (+) 0 [1,2,3] = 6 

prod' :: Num a => (a->a->a) -> a -> [a] ->a 
prod' _ total [] = total
prod' fn total (x:xs) = prod' fn (total *  x)  xs  -- infix notation For prefix notation -> prod' fn (fn total x) xs  

-- The above functions are nothing but extensions of folds
--       accumulator fn   
foldL :: (a -> b -> a) -> a -> [b] -> a 
foldL fn acc [] = acc -- acc is of type a
foldL fn acc (x:xs) = foldL fn (fn acc x) xs

-- To call :  foldL (+) 0 [1,7,7]. Also foldL (\acc x -> acc + x) 0 [1,7,7] => Lambda expressions  
-- foldL (*) 1 [1,7,7] = foldL (\acc x -> acc * x) 1 [1,7,7] => Lambda expressions

-- search if a element is present in a list
elem' :: (Eq a) => a -> [a] -> Bool 

elem' _ [] = False
elem' x (n:ns) 
       | x == n = True  
       | otherwise = elem' x ns 
-- elem' x (n:ns) = x == n || elem' x ns 

-- remove duplicate elements in a list
nub' :: (Eq a) => [a] -> [a]
nub' [] = []
nub' (a:as) 
      | not $elem' a as  =  a : nub' as   
      | otherwise = nub' as 

-- isAsc checks if the list is in a ascending order
isAsc':: [Int] -> Bool 

-- The first two conditions get evaluated first in the pattern matching
isAsc' [] = True 
isAsc' [a] = True 
isAsc' (a:as) = a <= head as && isAsc' as  


-- reverse a list using foldr or foldl
rev :: [a] -> [a]
rev = foldl (\acc x -> x : acc) []

rev' :: [a] -> [a]
rev' = foldr(\x acc -> acc ++ [x]) []

-- -- display 1st n elements in a list
-- displayFirstElements :: Int n => n -> [a] -> [a]
-- displayFirstElements n = 

