module CountDown where 
import Data.List (sortBy)
import Data.Ord (comparing)

--- Defining all possible operators
data Op = Add | Sub | Mul | Div |Exp   

-- Make the Op datatype instance of show so that it can be printed on the screen
instance Show Op where 
     show Add = "+"
     show Sub = "-"
     show Mul = "*"
     show Div = "/"
     show Exp = "^"


instance Eq Op where
     Add == Add = True
     Sub == Sub = True
     Mul == Mul = True
     Div == Div = True
     Exp == Exp = True
     _ == _ = False 

-- Define valid operations
valid :: Op -> Int -> Int -> Bool
valid Add x y = x<= y 
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y =  y/= 0 && y /= 1 && x `mod` y == 0
valid Exp x y =  x /= 1 && y > 1

-- apply a given operator to the two integers
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y 


-- Define a recursive datatype called Expr
data Expr = Val Int | App Op Expr Expr

instance Show Expr where
    show (Val n) = show n
    show (App o lexpr rexpr) = break lexpr ++ show o ++ break rexpr
                                where break e = "(" ++ show e  ++ ")" 

-- Extract all the values from the Expr
values :: Expr -> [Int]
values (Val n) = [n]
values (App _ lexpr rexpr) =  values lexpr ++ values rexpr

-- evaluate a given expression to a singleton int list if successful otherwise return a null list 
eval :: Expr -> [Int]
eval (Val n) = [n]
eval (App o lexpr rexpr) = [ apply o x y | x <- eval lexpr, y <- eval rexpr, valid o x y] 


-- COMBINATORIAL FUNCTIONS

-- define the superset of a given set
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) =  subs xs ++ map (x:) (subs xs) 

-- returns the list of all possible ways/orders of inserting a element into a given list
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

-- define a function that returns the permuations of a given list
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = [zs | ys  <- perms xs, zs <- interleave x ys] 

-- define a function that returns the permutations of the superset of a given list
choices :: [a] -> [[a]]
choices [] = [[]]
choices xs = [zs | ys <- subs xs, zs <- perms ys]


-- FORMALISE THE PROBLEM

type Result = (Expr, Int)

splitInToTwo :: [a] -> [([a],[a])]
splitInToTwo [] = []
splitInToTwo [_] = []
splitInToTwo (x:xs) = ([x],xs) : [(x:ls, rs) | (ls, rs) <- splitInToTwo xs]

ops :: [Op]
ops =[Add, Sub, Mul, Div, Exp]

combine :: Result -> Result -> [Result]
combine (lexpr, x) (rexpr, y)  =  [(App o lexpr rexpr , apply o x y ) | o <- ops, valid o x y]

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n)]
results list = [res |  (ls, rs) <- splitInToTwo list,
                        lx   <- results ls,  
                        ry   <- results rs, 
                        res  <- combine lx ry  
                ] 

-- Generate all possible solutions for the given list and target integer
solutions :: [Int] -> Int -> [Expr] 
solutions list target = [ expr | ls <- choices list, (expr, value) <- results ls, value == target]

-- IF SOLUTION TO A PROBLEM DOES NOT EXIST

-- define a function that gets allpossible expressions and difference between the actual and target value
allpossiblesoldiff :: [Int] -> Int -> [Result]
allpossiblesoldiff list target = [(expr, abs (value - target))| ls <- choices list, (expr, value) <- results ls]

-- sort all possible sol differences according to the absolute value of difference between the actual and target value
sortallpossiblesol :: [Int] -> Int -> [Result]
sortallpossiblesol list target =  sortBy (comparing snd) $ allpossiblesoldiff list target 

-- Find the nearest solution by taking the head of the list
nearestsolution :: [Int] -> Int -> Result
nearestsolution list target = head (sortallpossiblesol list target)

-- ORDERING SOLUTIONS USING COMPLEXITY

type ResultsWithComplexity = [(Expr, Int)]
type EfficientResult = (Expr, Int)


getcomplexityscore :: Expr -> Int
getcomplexityscore (Val n) = 0
getcomplexityscore (App Add l r) = 1 + getcomplexityscore l + getcomplexityscore r
getcomplexityscore (App Sub l r) = 1 + getcomplexityscore l + getcomplexityscore r
getcomplexityscore (App Mul l r) = 10 + getcomplexityscore l + getcomplexityscore r
getcomplexityscore (App Div l r) = 10 + getcomplexityscore l + getcomplexityscore r
getcomplexityscore (App Exp l r) = 100 + getcomplexityscore l + getcomplexityscore r


ordersolutionsbycomplexity :: [Int] -> Int -> ResultsWithComplexity
ordersolutionsbycomplexity list target = sortBy (comparing snd) [(expr, getcomplexityscore expr) |  expr  <- solutions list target] 

getsimplestsolution :: [Int] -> Int -> EfficientResult
getsimplestsolution list target = head (ordersolutionsbycomplexity list target)  



-- Examples for quick test

sol1 :: [Expr]
sol1 = solutions [1,3,7,10,25,50] 765

sol2 :: EfficientResult
sol2 = getsimplestsolution [1,3,7,10,25,50] 765

-- no actual solution exists for this problem
sol3 :: Result
sol3 = nearestsolution [1,3,7,10,25,50] 831


main :: IO ()
main = do putStr "Solutions for the problem:"
          print sol1 
          putStr "No of solutions that exist:"
          print (length sol1)
          putStr "Simplest solution:"
          print (fst sol2)
          putStr "with complexity score:" 
          print (snd sol2) 
          putStr "Nearest solution:"
          print sol3
          putStr "with absolute difference:" 
          print (snd sol3)
