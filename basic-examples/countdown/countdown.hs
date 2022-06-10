module CountDown where 

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


sol :: [Expr]
sol = solutions [1,3,7,10,25,50] 765

main :: IO ()
main = do print sol 
          print (length sol)   