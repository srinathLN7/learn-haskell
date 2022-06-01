module TautologyChecker where 


data Prop = Const Bool  
            | Var Char
            | Not Prop 
            | And Prop Prop
            | Or Prop Prop 
            | Imply Prop Prop
            | Eq Prop Prop 


-- Type declarations can also be with more than one parameters.
-- For example, a type of lookup tables that associate keys of one type to values of another type can be declared as a list of (key,value) pairs
type Assoc k v = [(k, v)]

-- Define a lookup table Subst which maps a Char to a Bool value
-- Eg: [('A', False), ('B', True)]  
type Subst = Assoc Char Bool

-- find - outputs a value associated with the given key in the lookup table if it exists
find :: Eq a => a -> [(a,b)] -> b
find ch xs = head ([ v | (k, v) <- xs, ch == k])

--  eval function evaluates a proposition to a Bool value by substituing the variables with the Bool values associated with them 
eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And p1 p2) = eval s p1 && eval s p2
eval s (Or p1 p2) = eval s p1 || eval s p2
eval s (Imply p1 p2) = eval s p1 <= eval s p2
eval s (Eq p1 p2) = eval s p1 == eval s p2
 
-- vars function extracts all the variables in the given proposition
vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p 
vars (And p1 p2) = vars p1 ++ vars p2
vars (Or p1 p2) = vars p1 ++ vars p2
vars (Imply p1 p2) = vars p1 ++ vars p2
vars (Eq p1 p2) = vars p1 ++ vars p2


-- bools outputs all possible combinations 2^n of bool values for a given int n 
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss 
          where bss = bools (n-1)

-- rmdups removes all duplicate elements in the list
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x: rmdups(filter(/= x) xs) 

-- substs given a proposition it outputs all possible results by subtituting all possible variable combinations 
substs :: Prop -> [Subst] 
substs p = map (zip vs) $ bools (length vs)
        where vs = rmdups (vars p)


-- isTuat evaluates the given proposition for all possible boolean value combinataions and returns true only if all possible combinations are true 
isTaut :: Prop -> Bool
isTaut p = and[eval s p | s <- substs p]


-- Some propositions for quick test
p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

p5 :: Prop
p5 = Or (Var 'A') (Var 'B')

p6 :: Prop
p6 = Or (Var 'A') (Not (Var 'A'))

p7 :: Prop
p7 = And (Var 'A') (Var 'A')

p8 :: Prop
p8 = Eq (Var 'A') (Var 'A')