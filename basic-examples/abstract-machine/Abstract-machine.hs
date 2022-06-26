module AbstractMachine where 

-- Define the Expr as a new datatype 
data Expr = Val Int | Add Expr Expr | Mult Expr Expr

-- Define the list of all possible operations in the control stack
data Op = EVALA Expr | ADD Int | EVALM Expr | MULT Int 

-- Define a control stack with a list of operations
-- Rem type is just a synonym or alias for an existing type Eg: type String = [Char]
type Cont = [Op]

---  MUTUAL RECURSSION ---

-- eval takes in an expression and the control stack and outputs an int
eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVALA y : c)
eval (Mult x y) c = eval x (EVALM y : c)


-- exec takes in the list of operations in the control stack and an integer and outputs an int
exec :: Cont -> Int -> Int
exec [] n = n
exec (EVALA y :c) n = eval y (ADD n :c)
exec (ADD n :c) m = exec  c (n+m) 
exec (EVALM y :c) n = eval y (MULT n :c)
exec (MULT n :c) m = exec c (n*m)


-- result outputs the final value of the given expression
result :: Expr -> Int 
result expr = eval expr []

-- Some examples for quick test
expr1 :: Expr
expr1 = Add ( Add (Val 2) (Val 3)) (Val 4)

expr2 :: Expr
expr2 = Mult ( Add (Val 2) (Val 3)) (Val 4)

expr3 :: Expr
expr3 = Mult (Add (Val 2) (Val 3)) (Add (Val 2) (Val 3))
