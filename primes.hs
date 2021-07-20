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

