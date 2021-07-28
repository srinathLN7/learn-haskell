
import Data.List ( sort )
import Data.Char(ord, chr)

x :: [Char]
x = "Hello Haskel!!! I am here"

-- define a function 
evenOrOdd :: Integral x => x -> [Char]

-- x is a evenOrOdd function
-- evenOrOdd x = if  even x then "Even" else "Odd"

-- evenOrOdd x
--     | even x = "Even"
--     | otherwise  = "Odd"

evenOrOdd 0 = "Even" -- base case
evenOrOdd 1 = "Odd" -- base case
evenOrOdd x = evenOrOdd(mod x 2)

-- bmi :: (Fractional a, Ord a) => a -> a -> [Char]
-- gaurded equations - better alternative to if else if
-- Use if else only for one cases
bmi :: Float -> Float  -> String 
bmi w h
    | bmi_calc <= 18.5 = "Underweight"
    | bmi_calc <= 25.0 = "Normal"
    | bmi_calc > 25.0 = "Overweight"
    | otherwise = "Invalid"
    where bmi_calc=w/h^2


-- gravityFlip:: Char -> [Int] -> [Int]
-- -- gravityFlip x list 
-- --         | x == 'R' = sort list
-- --         | x == 'L' = reverse(sort list)

-- gravityFlip 'R' = sort
-- gravityFlip 'L' = reverse .sort  

greet :: String -> [Char ]
greet name = concat ["Hello, ", name, " how are you doing?"]


-- removeExclamation str = [ch | ch <- str, ch /= '!'] 

removeExclamationAlt = filter (/= '!') 

-- repeatString n str = concat [str | _ <- [1..n]]

-- f(g(h(x))) = f (g (h x))
--- Alternatively let a = h x , b = g a , c = f b 
-- repeatStr n str = concat (take n (repeat str))

repeatStr n str = concat (replicate n str) --repeatStr n str = concat $ replicate  n str

repeatStr' n = concat. replicate n

sumStr a b = show $ readInt a + readInt b -- show (readInt a + readInt b)

readInt :: String -> Int 
readInt "" = 0
readInt a = read a :: Int  

doubleMe :: Num  a => a -> a
doubleMe n = n*2 


fib :: Int  -> Int  
fib 0 = 0
fib 1 = 1

fib n
    | n >0 = fib (n-1) + fib (n-2)
    | otherwise =0

greet' ::  String ->  String-> String 
greet' name owner 
       | name == owner = "Hello boss"
       | otherwise = "Hello guest"    

powersOf2 n = map (2^) [0..n]



firstNonConsecutive :: (Eq a, Enum a) => [a] -> Maybe a

-- firstNonConsecutive (a:b:ab) = if succ a == b then firstNonConsecutive(b:ab) else Just b 
firstNonConsecutive (a:b:ab)
            | succ a == b = firstNonConsecutive(b:ab)
            | otherwise = Just b
firstNonConsecutive _ = Nothing -- [], [x] = Nothing -> _ represents any other case that is not mentioned in the guard equations 

-- display the triplets (x,y,z) where z62 = x^2 + y^2 eg: (3,4,5)
tripletsWithin n = [ (i-1)^2 + (i-2)^2 == i^2 | i <- [3..n]]

-- Defining a custom data type -- Records
data Player = Player {team:: String, ppg:: Double } deriving (Show) -- :t team Player -> String :t ppg Player -> Double

sumPpg :: Player -> Player -> Double
sumPpg p1 p2 = ppg p1 + ppg p2 

sumPpg' (Player _ ppg) (Player _ ppg') = ppg + ppg' -- same as  (Player name ppg) (Player name' ppg')

strEndsWith :: String -> String -> Bool 
strEndsWith str1 str2 
        | null str1 || null str2 = False  --edge cases always need to come first. Pattern matching happens in order.
        | last str1 == last str2 = True   
        | otherwise = False  


-- Build a cipher and decipher it
cipher :: [Char] -> Int -> [Char]
cipher str offSet = [ chr (ord s + offSet)  | s <- str]



-- oddOrEvenSum

oddOrEvenSum :: Integral a => [a] -> String 
oddOrEvenSum list
        | odd (sum list) = "odd"  -- odd (sum' list) will also work
        | otherwise  = "even"


-- user-defined data types
--data Point = Point {a :: Double, b :: Double} deriving(Show) 
data Point = Point Double Double deriving(Show) 

add :: Point -> Point -> Point 
add (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)
-- add p1 p2 = Point (a p1 + a p2) (b p1 + b p2) 

dot :: Point -> Point -> Double
dot (Point x1 y1) (Point x2 y2) = x1 * x2 + y1 * y2

nbYear :: Int -> Double -> Int -> Int -> Int 
nbYear p0 r aug p
        | p0 >= p  = 0
        | otherwise = nbYear newP0 r aug p
        where newP0 =  floor (fromIntegral p0 * (1 + r/100.0)) + aug 

lastSurvivor ::  String -> [Int] -> Char 
lastSurvivor x [] = head x --lastSurvivor [x] [] = x
lastSurvivor x (a:as) =  lastSurvivor(take a x ++  drop (a + 1) x) as     -- See take and drop functions for the list 

pdtOfSum :: [[Int]] -> Int 
pdtOfSum = product . map sum -- Using curried functions here. Same as pdtOfSum list = product $map sum list

pdtOfSum' :: [[Int]] -> Int 
pdtOfSum' = foldl (\acc list -> acc * sum list) 1  --foldl (b -> a -> b) b (t a) pdtOfSum' list = foldl (\acc list -> acc * sum list) 1 list

-- Divide and conquer example - To solve the big problems - solve the sub-problems
hscale ::  Int-> [Char] -> [Char]
hscale k str =  concat  [replicate k ch | ch <- str ] -- different from  replicate k [ch | ch <- str]) = replicate k str

vscale :: Int -> [[Char]] -> [[Char]]
vscale n strArr =  concat [replicate n str | str <- strArr]

scale :: [Char] -> Int -> Int -> [Char]
scale "" _ _ = ""
scale str k n = foldl1(\acc str -> acc ++ "\n" ++ str) $vscale n $ map (hscale k) (words str) 


-- Construct an ascending order list
asc :: Int -> Int -> [Int]
asc a b 
    | b < a = []
    | b == a = [a]
    | otherwise = a:  asc (a +1) b  

-- Tail Recurrsive functions  -- Can do infinite recursions
fact n = aux n 1 where aux n acc 
                        | n <= 1 = acc 
                        | otherwise = aux (n-1) $ n * acc 


addTuples  :: [(Int, Int)] -> [Int]
addTuples xs = [ x + y | (x, y) <- xs]


getTupleX :: [(Int, Int)] -> [Int]
getTupleX xs = [x | (x, _) <- xs]


getTupleY :: [(Int, Int)] -> [Int]
getTupleY xs = [y | (_, y) <- xs]

addTuplesComponent ::  [(Int, Int)] ->  [(Int, Int)]
addTuplesComponent n = [(x_sum, y_sum)] 
        where 
                x_sum = sum $getTupleX n 
                y_sum = sum $getTupleY n


-- Directed cyclic graph problem - combination of all other problems

hasPath :: [(Int, Int)] -> Int -> Int -> Bool 
hasPath xy a b 
        | a ==b = True 
        | otherwise = elem a (getTupleX xy) && elem b (getTupleY xy)


-- Count the number of elements in the list
count e = foldr (\x acc -> if e == x then acc+1 else acc) 0   

-- length' = foldr (const $ (+) 1) 0

-- reverse a list using foldr or foldl
rev :: [a] -> [a]
rev = foldl (\acc x -> x : acc) []

rev' :: [a] -> [a]
rev' = foldr(\x acc -> acc ++ [x]) []

-- -- display 1st n elements in a list
-- displayFirstElements :: Int n => n -> [a] -> [a]
-- displayFirstElements n = 


-- display the list of prefixes of a list
prefixes:: [a] -> [[a]]
prefixes =  foldl (\acc x -> if null acc then [x]:acc else ( head acc ++ [x]) : acc) []

